#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "C:\\Users\\renso\\OneDrive\\Documentos\\DMEF" )
dataset  <- fread( "./exp/CLU1261/exp_CLU1261_cluster_de_bajas.txt" )


dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]