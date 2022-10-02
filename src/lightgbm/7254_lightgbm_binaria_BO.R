# source( "~/labo/src/lightgbm/z723_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("dplyr")
require("mlrMBO")
require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower=    0.01, upper=    0.1),
  makeNumericParam("feature_fraction", lower=    0.4  , upper=    1.0),
  makeIntegerParam("min_data_in_leaf", lower=    4000L   , upper=  6000L),
  makeIntegerParam("num_leaves",       lower=   800L   , upper=  1200L),
  makeIntegerParam("envios",           lower= 5000L   , upper= 15000L)
)

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "HT7252"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 388699  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 388699  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")

  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia, 
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )

  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]

  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global

  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
  )
  

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
                         )

    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }


  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( PARAM$input$dataset )


#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


variables_rank <- c("mcuentas_saldo","cdescubierto_preacordado", "cprestamos_personales", 
                    "mprestamos_personales", "mcuenta_corriente", "mpasivos_margen", "mcaja_ahorro", 
                    "mactivos_margen", "mcomisiones_mantenimiento", "mcomisiones", "mpayroll", "cpayroll_trx", "Visa_Fvencimiento",
                    "Master_Fvencimiento","Master_Finiciomora","Master_fultimo_cierre","Visa_fultimo_cierre","Visa_Finiciomora",
                    "mttarjeta_master_debitos_automaticos", "mtarjeta_visa_consumo", "Visa_msaldopesos",
                    "Visa_msaldototal", "Visa_mpagado", "Visa_mpagominimo", "Visa_mlimitecompra", "Visa_mfinanciacion_limite",
                    "mttarjeta_visa_debitos_automaticos", "mrentabilidad_annual", "mrentabilidad", "mcomisiones_otras",
                    "Master_mpagado","mtransferencias_recibidas", "matm_other", "matm", "mcheques_depositados", "ctransferencias_recibidas",
                    "Master_mpagosdolares", "Visa_mpagospesos", "Visa_msaldodolares", "mtransferencias_emitidas",
                    "Master_mconsumosdolares", "Master_msaldodolares", "mcaja_ahorro_dolares", "ccheques_depositados",
                    "mpagomiscuentas", "mtarjeta_visa_descuentos", "mplazo_fijo_dolares","mcajeros_propios_descuentos", "Master_msaldototal",
                    "mtarjeta_master_consumo", "mcuenta_debitos_automaticos", "cpagomiscuentas", "Master_msaldopesos", "Master_mconsumototal",
                    "Master_mpagominimo", "Visa_mconsumosdolares", "Visa_mpagosdolares", "Visa_mconsumototal"
)


for( campo in variables_rank )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), 0 ) ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), 0 ) ]
    dataset[   , paste0(campo) := NULL]
    variables_rank<-c(variables_rank, paste0( campo, "_neg" ))
    variables_rank<-c(variables_rank, paste0( campo, "_pos" ))
    variables_rank<-variables_rank[-which(variables_rank==campo)]
    
  }
}

dtrain  <- dataset[ foto_mes==202103 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202105 ]  #defino donde voy a aplicar el modelo



for (campo in variables_rank) {
  
  dtrain[    , paste0("rank_", campo) := (frankv(dtrain, cols = campo) - 1) / (length(dtrain[, get(campo)]) - 1)] # rankeo entre 0 y 1
  dtrain[   , paste0(campo) := NULL] 
}      


for (campo in variables_rank) {
  
  dapply[    , paste0("rank_", campo) := (frankv(dapply, cols = campo) - 1) / (length(dapply[, get(campo)]) - 1)] # rankeo entre 0 y 1
  dapply[    , paste0(campo) := NULL] 
}      


dataset<-merge(dtrain,dapply, all=TRUE)


dataset[ , campo1 := as.integer( ctrx_quarter <20 & rank_mcuentas_saldo_neg < 0.17933 & rank_mprestamos_personales <0.79794 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <20 & rank_mcuentas_saldo_neg < 0.17933 & rank_mprestamos_personales >=0.79794 ) ]

dataset[ , campo3 := as.integer( ctrx_quarter <20 & rank_mcuentas_saldo_neg >= 0.17933 & rank_mpasivos_margen_pos < 0.12563 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <20 & rank_mcuentas_saldo_neg >= 0.17933 & rank_mpasivos_margen_pos >= 0.12563 ) ]

dataset[ , campo5 := as.integer( ctrx_quarter>=20 & ctrx_quarter<41  & rank_mpasivos_margen_pos<0.15352  ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=20 & ctrx_quarter<41  & rank_mpasivos_margen_pos>=0.15352   ) ]

dataset[ , campo7 := as.integer( ctrx_quarter>=20 & ctrx_quarter>=41  & rank_cpayroll_trx<0.42177  ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=20 & ctrx_quarter>=41  & rank_cpayroll_trx>=0.42177 ) ]


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% PARAM$input$training & 
          ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

