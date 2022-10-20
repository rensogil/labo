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
PARAM$experimento  <- "Ganancia_FE_Combinaciones_1"

PARAM$input$dataset       <- "./datasets/competencia3_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-    31
PARAM$finalmodel$learning_rate     <-    0.014158611586616   #0.0142501265
PARAM$finalmodel$num_iterations    <-    669  #615
PARAM$finalmodel$num_leaves        <-    909 #784
PARAM$finalmodel$min_data_in_leaf  <-    62  #5628
PARAM$finalmodel$feature_fraction  <-   0.421920091063418  #0.8382482539
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

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

#Relación con edad
dataset[ , mcuentas_edad := mcuentas_saldo / cliente_edad ]
dataset[ , mprestamos_personales_edad := mprestamos_personales / cliente_edad ]
dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

#creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

## Tiempo de vida en el banco 
dataset[, vida_banco := (cliente_antiguedad/12) / (cliente_edad)]

## Ganancias banco
dataset[, mmargen := rowSums(.SD, na.rm = TRUE), .SDcols = c("mactivos_margen", "mpasivos_margen")]
dataset[, mmargen_x_producto := mmargen / cproductos]

## Total Pasivos
dataset[, total_deuda := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "Visa_msaldototal", "Master_msaldototal")]

## Total Activos
dataset[, total_activos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mcuentas_saldo")]

## Balance en el banco
dataset[, balance := total_activos - total_deuda]
dataset[, ratio_deuda := total_deuda / (total_activos + 1)]

# saldos
dataset[, has_cuentacorriente_saldo_pos := ifelse(mcuenta_corriente > 0, 1, 0) ]
dataset[, has_cajaahorro_saldo_pos := ifelse(mcaja_ahorro > 0, 1, 0) ]
dataset[, has_saldo_pos := ifelse(mcaja_ahorro + mcuenta_corriente > 0, 1, 0) ]

## Tiene cuenta homebanking
dataset[, has_internet := ifelse(dataset$internet > 0, 1, 0) ]

## Tiene movimientos/tarjetas
dataset[, has_debito_transacciones := ifelse(dataset$ctarjeta_debito_transacciones > 0, 1, 0) ]
dataset[, has_visa := ifelse(dataset$ctarjeta_visa > 0, 1, 0) ]
dataset[, has_visa_transacciones := ifelse(dataset$ctarjeta_visa_transacciones > 0, 1, 0) ]
dataset[, has_master := ifelse(dataset$ctarjeta_master > 0, 1, 0) ]
dataset[, has_master_transacciones := ifelse(dataset$ctarjeta_master_transacciones > 0, 1, 0) ]

## Cantidad de tarjetas total
dataset[, ctarjetas := rowSums(.SD, na.rm = TRUE), .SDcols = c("ctarjeta_visa", "ctarjeta_master")]

## Total seguros
dataset[, cseguros := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

## Recibo pago de sueldo?
dataset[, has_payroll := ifelse(dataset$cpayroll_trx + dataset$cpayroll2_trx  > 0, 1, 0) ]

## Total payroll
dataset[, mpayroll_total := mpayroll + mpayroll2]

## Tiene débitos automáticos?
dataset[, has_da := ifelse(dataset$ccuenta_debitos_automaticos + dataset$ctarjeta_visa_debitos_automaticos + dataset$ctarjeta_master_debitos_automaticos  > 0, 1, 0) ]

## cant pago mis cuentas?
dataset[, has_pmc := ifelse(dataset$cpagomiscuentas  > 0, 1, 0) ]

## Total débitos automáticos
dataset[, debitos_automaticos := mcuenta_debitos_automaticos + mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos]

## Total Consumos y gastos
dataset[, total_consumos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", "mpagodeservicios", "debitos_automaticos")]

## Total descuentos (sobre total de consumos?)
dataset[, total_descuentos := rowSums(.SD, na.rm = TRUE), .SDcols = c("mtarjeta_visa_descuentos", "mtarjeta_master_descuentos", "mcajeros_propios_descuentos")]

## Descuentos sobre consumos
dataset[, total_descuentos_sobre_consumos := ifelse(dataset$total_consumos == 0, 0, total_descuentos / total_consumos)]

## Total comisiones
dataset[, has_comisiones := ifelse(dataset$ccomisiones_mantenimiento + dataset$ccomisiones_otras > 0, 1, 0) ]
dataset[, total_comisiones := mcomisiones_mantenimiento + mcomisiones_otras]

## Balance transferencias
dataset[, balance_transferencias := mtransferencias_recibidas - mtransferencias_emitidas]

## ¿hace más transacciones en cajeros de otros bancos?
dataset[, cajeros_ajenos := ifelse(dataset$matm < dataset$matm_other, 1, 0)]

## ctrx quarter / cantidad de productos?
dataset[, ctrx_x_producto := ctrx_quarter / cproductos]

## comisiones / ctrx_quarter?
dataset[, comisiones_x_trx := total_comisiones / (ctrx_quarter + 1) ]

# fechas tarjetas: llevo a años:
dataset[, master_vencimiento := floor(dataset$Master_Fvencimiento/365)]
dataset[, master_alta := floor(dataset$Master_fechaalta/365)]
dataset[, visa_vencimiento := floor(dataset$Visa_Fvencimiento/365)]
dataset[, visa_alta := floor(dataset$Visa_fechaalta/365)]

## limite de compra promedio
dataset[, limite_compra_promedio := ifelse(dataset$ctarjetas == 0, 0, mv_mlimitecompra / ctarjetas) ]

# pagado sobre saldo
dataset[, pagado_sobre_saldo := ifelse(dataset$mv_msaldototal == 0, 0, mv_mpagado / mv_msaldototal) ]

# consumo promedio
dataset[, consumo_promedio := ifelse(dataset$mv_cconsumos == 0, 0, mv_mconsumototal /  mv_cconsumos) ]

## limite de compra sobre ingresos
dataset[, limite_compra_sobre_ingresos := ifelse(dataset$mpayroll_total == 0, NA, mv_mlimitecompra / mpayroll_total) ]
dataset[, limite_compra_sobre_activos := ifelse(dataset$total_activos == 0, NA, mv_mlimitecompra / total_activos) ]

## limite de compra real vs esperado según ingreso
limite_esperado = median(dataset[mpayroll_total > 0, mv_mlimitecompra / mpayroll_total], na.rm=TRUE)
dataset[, limite_compra_real_sobre_esperado := ifelse(dataset$total_activos == 0, NA, mpayroll_total * limite_esperado - mv_mlimitecompra) ]

## suma de columnas null
dataset$na_count <- rowSums(is.na(dataset))

#valvula de seguridad para evitar valores infinitos (paso los infinitos a NULOS)
infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  dataset[mapply(is.infinite, dataset)] <- NA
}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <- 0
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
