#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLU1262"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
dataset  <- fread( "./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)

#combino MasterCard y Visa
dataset[, mv_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]
dataset[, mv_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
dataset[, mv_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
dataset[, mv_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
dataset[, mv_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
dataset[, mv_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
dataset[, mv_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
dataset[, mv_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
dataset[, mv_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
dataset[, mv_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
dataset[, mv_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
dataset[, mv_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
dataset[, mv_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
dataset[, mv_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
dataset[, mv_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
dataset[, mv_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
dataset[, mv_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
dataset[, mv_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
dataset[, mv_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
dataset[, mv_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

## Ganancias banco
dataset[, mmargen := rowSums(.SD, na.rm = TRUE), .SDcols = c("mactivos_margen", "mpasivos_margen")]

## Total Pasivos
dataset[, total_deuda := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "Visa_msaldototal", "Master_msaldototal")]

## Total Activos
dataset[, total_activos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mcuentas_saldo")]

## Balance en el banco
dataset[, balance := total_activos - total_deuda]


## Cantidad de tarjetas total
dataset[, ctarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("ctarjeta_visa", "ctarjeta_master")]

## Total seguros
dataset[, cseguros := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

## Total payroll
dataset[, mpayroll_total := mpayroll + mpayroll2]

## Total débitos automáticos
dataset[, debitos_automaticos := mcuenta_debitos_automaticos + mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos]

## Total Consumos y gastos
dataset[, total_consumos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", "mpagodeservicios", "debitos_automaticos")]

## Balance transferencias
dataset[, balance_transferencias := mtransferencias_recibidas - mtransferencias_emitidas]

## Total Consumos y gastos
dataset[, total_consumos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", "mpagodeservicios", "debitos_automaticos")]

## Total descuentos (sobre total de consumos?)
dataset[, total_descuentos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mtarjeta_visa_descuentos", "mtarjeta_master_descuentos", "mcajeros_propios_descuentos")]

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mv_mconsumospesos", "mv_mconsumosdolares",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                    "cliente_edad", "chomebanking_transacciones","mrentabilidad", "mcuenta_corriente", "mcomisiones_mantenimiento", "cliente_antiguedad",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "ccomisiones_otras", "mtransferencias_emitidas","mpagomiscuentas", "mv_msaldototal",
                    "mv_mlimitecompra", "mv_mconsumototal", "mmargen", "total_deuda", "total_activos", "balance", "mpayroll_total",
                    "debitos_automaticos", "total_consumos", "total_descuentos", "balance_transferencias")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )


#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=2 & distintos <=3 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )

dataset[  , mean(cliente_antiguedad),  cluster2 ]


dataset[  , mean(cliente_edad),  cluster2 ]
dataset[  , min(cliente_edad),  cluster2 ]
dataset[  , max(cliente_edad),  cluster2 ]

dataset[  , min(cliente_antiguedad),  cluster2 ]
dataset[  , max(cliente_antiguedad),  cluster2 ]
dataset[  , mean(balance),  cluster2 ]
dataset[  , mean(balance_transferencias),  cluster2 ]

dataset[  , mean(mtransferencias_recibidas),  cluster2 ]
dataset[  , mean(mtransferencias_emitidas),  cluster2 ]

dataset[  , mean(cpayroll_trx),  cluster2 ]

dataset[  , mean(mpayroll),  cluster2 ]

dataset[  , mean(mpayroll2),  cluster2 ]

dataset[  , mean(total_consumos),  cluster2 ]

dataset[  , mean(total_consumos),  cluster2 ~ foto_mes ]


resumen <- dcast.data.table(dataset, cluster2 ~ foto_mes, fun.aggregate = mean,
                            value.var = 'total_consumos')

resumen <- dcast.data.table(dataset12, cluster2 ~ foto_mes, fun.aggregate = mean,
                            value.var = 'ctrx_quarter')
resumen