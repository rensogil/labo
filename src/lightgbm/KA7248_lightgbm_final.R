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
PARAM$experimento  <- "KA7248"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

# Utilizo el 1° valor de ganancia de HT7244 iteracion 150
PARAM$finalmodel$max_bin           <-   69
PARAM$finalmodel$learning_rate     <-   0.0944009364011443   #0.0142501265
PARAM$finalmodel$num_iterations    <-   129  #615
PARAM$finalmodel$num_leaves        <-   665 #784
PARAM$finalmodel$min_data_in_leaf  <-   758  #5628
PARAM$finalmodel$feature_fraction  <-   0.31391569985389  #0.8382482539
PARAM$finalmodel$max_depth         <-   10
PARAM$finalmodel$min_gain_to_split <-   0.0
PARAM$finalmodel$lambda_l1         <-   8.31434093521441
PARAM$finalmodel$lambda_l2         <-   46.7136180289771
PARAM$finalmodel$semilla           <- 388699

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "~/buckets/b1" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

#---------------------------------------
dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]

dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]

dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ctrx_quarter<30  & mcaja_ahorro<2604.3  ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ctrx_quarter<30  & mcaja_ahorro>=2604.3  ) ]

dataset[ , campo7 := as.integer( ctrx_quarter>=14 & ctrx_quarter>=30  & ctrx_quarter<49  ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=14 & ctrx_quarter>=30  & ctrx_quarter>=49  ) ]


#-------------------------------
mis_variables <- c("ctrx_quarter","mcaja_ahorro", "mpayroll","mcuentas_saldo", "mpasivos_margen",
                   "mtarjeta_visa_consumo", "mprestamos_personales","mrentabilidad_annual",
                   "Visa_msaldototal", "mcuenta_corriente", "mactivos_margen", "mcomisiones_mantenimiento",
                   "mrentabilidad", "cdescubierto_preacordado", "mcomisiones_otras", "cliente_edad",
                   "Visa_mpagominimo", "Master_fechaalta", "Master_Fvencimiento", "Visa_fechaalta",
                   "ctarjeta_visa", "Master_status", "Visa_msaldopesos", "Visa_Fvencimiento",
                   "cprestamos_personales", "cproductos", "chomebanking_transacciones", "ctarjeta_visa_transacciones",
                   "Visa_mconsumospesos", "mcomisiones"
)


variables_rank <- c("mcuentas_saldo","cdescubierto_preacordado", "cprestamos_personales", 
                    "mprestamos_personales", "mcuenta_corriente", "mpasivos_margen", "mcaja_ahorro", 
                    "mactivos_margen", "mcomisiones_mantenimiento", "mcomisiones", "mpayroll", "cpayroll_trx", "Visa_Fvencimiento",
                    "Master_Fvencimiento","Master_Finiciomora","Master_fultimo_cierre","Visa_fultimo_cierre","Visa_Finiciomora",
                    "mttarjeta_master_debitos_automaticos", "mtarjeta_visa_consumo", "Visa_msaldopesos",
                    "Visa_msaldototal", "Visa_mpagado", "Visa_mpagominimo", "Visa_mlimitecompra", "Visa_mfinanciacion_limite",
                    "mttarjeta_visa_debitos_automaticos", "mrentabilidad_annual", "mrentabilidad", "mcomisiones_otras",
                    "Master_mpagado","mtransferencias_recibidas", "matm_other", "atm", "mcheques_depositados", "ctransferencias_recibidas",
                    "Master_mpagosdolares", "Visa_mpagospesos", "Visa_msaldodolares", "mtransferencias_emitidas",
                    "Master_mconsumosdolares", "Master_msaldodolares", "mcaja_ahorro_dolares", "ccheques_depositados",
                    "mpagomiscuentas", "mtarjeta_visa_descuentos", "mplazo_fijo_dolares","mcajeros_propios_descuentos", "Master_msaldototal",
                    "mtarjeta_master_consumo", "mcuenta_debitos_automaticos", "cpagomiscuentas", "Master_msaldopesos", "Master_mconsumototal",
                    "Master_mpagominimo", "Visa_mconsumosdolares", "Visa_mpagosdolares", "Visa_mconsumototal"
)

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
  
}

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
                                   max_depth=          PARAM$finalmodel$max_depth,
                                   min_gain_to_split=  PARAM$finalmodel$min_gain_to_split,
                                   lambda_l1=          PARAM$finalmodel$lambda_l1,
                                   lambda_l2=          PARAM$finalmodel$lambda_l2,
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
