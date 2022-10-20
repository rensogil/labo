# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("dplyr")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "Ganancia_FE_APP_1"

PARAM$input$dataset       <- "./datasets/competencia3_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-    31
PARAM$finalmodel$learning_rate     <-    0.00668145186425275   #0.0142501265
PARAM$finalmodel$num_iterations    <-    1476  #615
PARAM$finalmodel$num_leaves        <-    877 #784
PARAM$finalmodel$min_data_in_leaf  <-   1243  #5628
PARAM$finalmodel$feature_fraction  <-   0.209542028533842  #0.8382482539
PARAM$finalmodel$semilla           <- 388699

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

dataset[ , campo1 := as.integer( mcaja_ahorro <312.02 & mtarjeta_visa_consumo < 815.3 & mprestamos_personales <14041 ) ]
dataset[ , campo2 := as.integer( mcaja_ahorro <312.02 & mtarjeta_visa_consumo < 815.3 & mprestamos_personales>=14041 ) ]

dataset[ , campo3 := as.integer( mcaja_ahorro <312.02 & mtarjeta_visa_consumo>= 815.3 & Visa_msaldototal <25849 ) ]
dataset[ , campo4 := as.integer( mcaja_ahorro <312.02 & mtarjeta_visa_consumo>= 815.3 & Visa_msaldototal>=25849 ) ]

dataset[ , campo5 := as.integer( mcaja_ahorro>=312.02 & cpayroll_trx< 1  & mtarjeta_visa_consumo<3946.4) ]
dataset[ , campo6 := as.integer( mcaja_ahorro>=312.02 & cpayroll_trx< 1  & mtarjeta_visa_consumo>=3946.4) ]

dataset[ , campo7 := as.integer( mcaja_ahorro>=312.02 & cpayroll_trx>=1  & mcomisiones_mantenimiento<1359.9  ) ]
dataset[ , campo8 := as.integer( mcaja_ahorro>=312.02 & cpayroll_trx>=1  & mcomisiones_mantenimiento>=1359.9  ) ]



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

ksemillas <- c(388699, 617153, 147263, 854417, 242807,306529, 472993, 669989, 775163, 996689)
envio <- c()
ganancias <-c()
semillas <- c()
for( semilla  in  ksemillas )
{
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
                                   seed=               semilla
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

#Defino el conjunto de evaluacion 
data_eval  <- dataset[ foto_mes== 202105  ]
data_eval <- data.frame(data_eval[,c("numero_de_cliente","clase_ternaria")])

#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )

for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  #
  tb <-tb_entrega[Predicted==1]
  tb <- tb %>% 
    left_join(., data_eval, by = "numero_de_cliente") 
  tb[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  ganancias <-c(ganancias,sum(tb$ganancia))
  envio <- c(envio, envios)
  semillas <- c(semillas, semilla)

  #fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
  #        file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
  #       sep= "," )
}
}

resultado_1 <- data.frame(
  "envios" = envio, 
  "semilla"= semillas,
  "ganancia" = ganancias
)  

Promedio_ganancias = resultado_1 %>%
  group_by(envios) %>%
  summarise(Promedio_ganancia = mean(ganancia), sd=sd(ganancia))
Promedio_ganancias_ord =Promedio_ganancias%>%
  arrange(desc(Promedio_ganancia))
Promedio_ganancias_ord

fwrite( Promedio_ganancias, 
        file= paste0(PARAM$experimento, ".csv" ),
      sep= "," )
#--------------------------------------

quit( save= "no" )
