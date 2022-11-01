#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9412"
PARAM$exp_input  <- "HT9412"

PARAM$modelos  <- 10
# FIN Parametros del script

ksemilla  <- 388699

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  arch_modelo  <- paste0( "modelo_" ,
                          sprintf( "%02d", i ),
                          "_",
                          sprintf( "%03d", iteracion_bayesiana ),
                          ".model" )


  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL

  #Utilizo la semilla definida en este script
  parametros$seed  <- ksemilla
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )

  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= arch_modelo )

  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( "impo_", 
                        sprintf( "%02d", i ),
                        "_",
                        sprintf( "%03d", iteracion_bayesiana ),
                        ".txt" ),
          sep= "\t" )


  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]


  nom_pred  <- paste0( "pred_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )


  #genero los archivos para Kaggle
  cortes  <- seq( from=  8000,
                  to=   15000,
                  by=     500 )


  setorder( tb_prediccion, -prob )

  for( corte in cortes )
  {
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )

  }


  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}

