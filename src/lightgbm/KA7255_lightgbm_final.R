# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")



#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7255"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

# Utilizo el 9 valor de ganancia de HT7250 iteracion 42
PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-     0.0620608068108074   #0.0142501265
PARAM$finalmodel$num_iterations    <-    191  #615
PARAM$finalmodel$num_leaves        <-   266  #784
PARAM$finalmodel$min_data_in_leaf  <-   479  #5628
PARAM$finalmodel$feature_fraction  <-   0.537479716740566  #0.8382482539
PARAM$finalmodel$semilla           <- 388699

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "~/buckets/b1" )

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


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )
