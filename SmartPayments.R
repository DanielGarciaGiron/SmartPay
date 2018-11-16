#Ciclo que se encarga de instalar o cargar librerias necesarias.
for (libreria in c("earth","dplyr","tidyr","caret","class")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#Se lee el archivo que contiene los datos
Datos <- read.csv("./PagosDetalle.csv", stringsAsFactors = F)

#Se separa la unica columna que se crea al cargar el documento en 4 columnas.
DatosLimpios <- Datos %>%
  separate(Fecha.ClienteDetalle.Monto.Paguelo, c("Fecha", "ClienteDetalle","Monto","Paguelo"), ";")

#Se ordenan los datos de acuerdo a la fecha y se resetean los indices de las filas.
DatosLimpios <- DatosLimpios[order(DatosLimpios$Fecha),]
rownames(DatosLimpios) <- 1:nrow(DatosLimpios)

#Se separa la columna de fecha en fecha, hora y si es en AM/PM.
DatosLimpios <- DatosLimpios %>%
  separate(Fecha, c("Fecha", "Hora","AM/PM"), " ")

#Esto crea un nuevo dataframe donde separa las filas que tengan un formato de 
#hora distinto al de los demas.
FechasDiferentes <- DatosLimpios[is.na(DatosLimpios$`AM/PM`),]

#se remueven las filas que se usaron para crear el dataset anterior
DatosFinales <- DatosLimpios[complete.cases(DatosLimpios),]

#se ordenan las fechas de forma ascendente y se resetean los indices de las filas
FechasDiferentes <- FechasDiferentes[order(FechasDiferentes$Fecha),]
rownames(FechasDiferentes) <- 1:nrow(FechasDiferentes)

#se eliminan estas filas porque les faltan la mayoria de datos.
FechasDiferentes <- FechasDiferentes[-c(1:14,75314:75320),]

#se separa la fecha en año, mes y dia.
FechasDiferentes <- FechasDiferentes %>%
  separate(Fecha, c("Año","Mes","Dia"), "-")

#se reorganizan las columnas para que este dataset se pueda agregar al original.
FechasDiferentes <- FechasDiferentes[,c(3,2,1,4,5,6,7,8)]

#se separa la fecha en dia, mes y año del dataset original
DatosFinales <- DatosFinales %>%
  separate(Fecha, c("Dia","Mes","Año"), "/")

#se juntan ambos datasets con todos los datos de manera uniforme.
DatasetFinal <- rbind(DatosFinales, FechasDiferentes)

#se convierten estas columnas en valores numericos para los metodos a utilizar.
DatasetFinal[, c(1,2,3,6,7)] <- sapply(DatasetFinal[, c(1,2,3,6,7)], as.numeric)

#se crea una variable numerica para describir el lugar donde se paga, esto se usa para 
#calculos
DatasetFinal$TipoCompra <- as.numeric(factor(DatasetFinal$Paguelo))

#Se crea un dataset temporal en el cual se calcula el pago promedio realizado en ese lugar.
#tambien se cuentan la cantidad de pagos efectuados en este establecimiento.
#se imprimen los gastos promedios. Esto se realiza 4 veces por que solo hay 4 lugares.
df <- DatasetFinal[DatasetFinal$Paguelo == "CLARO POSTPAGO",]
PromedioClaroPostpago <- mean(df$Monto)
CantidadClaro <- nrow(df)
print("Gasto promedio en Claro Postpago:")
PromedioClaroPostpago

df <- DatasetFinal[DatasetFinal$Paguelo == "EEGSA PAGO FACTURA",]
PromedioEegsapago <- mean(df$Monto)
CantidadEegsa <- nrow(df)
print("Gasto promedio en EEGSA:")
PromedioEegsapago

df <- DatasetFinal[DatasetFinal$Paguelo == "TIGO POSTPAGO",]
PromedioTigoPostpago <- mean(df$Monto)
CantidadTigo <- nrow(df)
print("Gasto promedio en Tigo Postpago:")
PromedioTigoPostpago

df <- DatasetFinal[DatasetFinal$Paguelo == "TIGO POSTPAGO/TIGO STAR",]
PromedioTigoStarPostpago <- mean(df$Monto)
CantidadTigoStar <- nrow(df)
print("Gasto promedio en Tigo Star Postpago:")
PromedioTigoStarPostpago


#se crea un diagrama de pie con los porcentajes de cada uno de los lugares de pago.
slices <- c(CantidadClaro, CantidadEegsa, CantidadTigo, CantidadTigoStar) 
lbls <- c("CLARO POSTPAGO", "EEGSA PAGO FACTURA", "TIGO POSTPAGO", "TIGO POSTPAGO/TIGO STAR")
pct <- round(slices/nrow(DatasetFinal)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Distribucion de pagos")




###########
#se utiliza el modelo mars, el cual es un modelo de regresion multivariable. 
#este predice el dia utilizando todos los datos (453275). Este modelo es de aprendizaje
#no supervisado.
mars<-earth(Dia~Mes+TipoCompra+Monto, data = DatasetFinal, pmethod = "backward",
            nprune = 10, nfold = 5)

#se imprime un resumen en el cual se dan los coeficcientes que definen al modelo y datos
#estadisticos acerca de que tan buenos son los resultados.
PrediccionesCompletas <- summary(mars, digit = 3)
PrediccionesCompletas

#se imprime un diagrama de densidad en el cual se muestra en que valor se concentraron los valores 
#predecidos. Este diagrama tiene dos lineas. La primera linea que dice dia == 1 significa que esos 
#valores fueron los que se predijeron y si fueron acertados, mientras que la linea que dice 
# dia != 1 se refiere a los valores que se predijeron pero no fueron acertados.
plotd(mars)

#######
#se toma una muestra del 65 por ciento de los datos.
set.seed(123)
porciento <- 65/100 
muestra<-sample(1:nrow(DatasetFinal),porciento*nrow(DatasetFinal))
TrainSet<-DatasetFinal[muestra,] 
TestSet<-DatasetFinal[-muestra,] 

#esta muestra se usa para obtener los knn y luego calcular la matriz de confusion.
predKnn<-knn(TrainSet[,c(2,7,9)],TestSet[,c(2,7,9)],as.factor(TrainSet$Dia),k=3)
cfm<-confusionMatrix(as.factor(TestSet$Dia),predKnn)
cfm

#Se genera el modelo lineal multivariable y se despliega el resumen de sus resultados
modeloLinealTotal<-lm(Dia~Mes+TipoCompra+Monto , data = TrainSet)
summary(modeloLinealTotal)

#Se realiza la prediccion utilizando los datos de prueba
prediccionTotal<-predict(modeloLinealTotal,newdata = TestSet[,c(2,7,9)])
prediccionTotal
#se calcula la resta entre el valor realy la prediccion
difTotal<-as.numeric(abs(prediccionTotal-TestSet$Dia))
difTotal
#se eliminan todos aquellos valores cuya prediccion fuera mayor a un +- 12 dias.
ResultadosBuenaPrediccion <- difTotal <- difTotal[difTotal < 12]
#se convierten los datos de double a integer. 
ResultadosBuenaPrediccion<-as.integer(ResultadosBuenaPrediccion)
#Este vector se convierte en un data frame para poder verse.
ResultadosPrediccionTotales = as.data.frame(ResultadosBuenaPrediccion)
#se abre el dataframe
View(ResultadosPrediccionTotales)

############
#se pide el id del cliente a estudiar.
IDcliente <- 9

#se crea un dataset solo con los valores de este cliente
DatosIndividuales <- subset(DatasetFinal[], DatasetFinal$ClienteDetalle == IDcliente)

#se utiliza el modelo lineal multivariable para predecir los dias a pagar.
modeloLinealMulti<-lm(Dia~Mes+TipoCompra+Monto , data = DatosIndividuales)

#se imprime el resumen con los coeficientes resultantes.
summary(modeloLinealMulti)

#se realiza la predicción de este modelo y se imprimen esos valores. La fila superior 
# se refiere al indice de la fila del dataset donde se tiene ese dato, mientras que la segunda
# es el dia de pago predicho.
prediccion<-predict(modeloLinealMulti,newdata = DatosIndividuales[,c(2,7,9)])
prediccion

#se calcula la diferencia entre el valor real y el valor predicho, se imprimen estos valores.
dif<-abs(prediccion-DatosIndividuales$Dia)
dif

#se imprime la desviacion estandar de estos datos. Este valor es la incertidumbre respecto al dia de pago.
desviacion <- summary(modeloLinealMulti)$coefficients[1,2]
desviacion


