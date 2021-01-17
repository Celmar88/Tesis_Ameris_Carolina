#---------------------------------------------------------------------------------
# Librerias necesarias
library(caret)
library(dplyr)

#---------------------------------------------------------------------------------

# Limpiamos memoria
rm(list=ls())

# Fijamos una semilla
set.seed(1006)

# Cargamos la data 
Ventas <- read.table("Ventas.txt", header = TRUE)

# Se prepara la data para las ventas Corriente.
Data_ACPM <- select(Ventas,1,4)

# Creamos el conjunto de datos de entrenamiento y prediccion.
Train <- createDataPartition(Data_ACPM$ACPM, p=0.85,list= F)
Entrenamiento <- Data_ACPM[Train,]
Prueba <- Data_ACPM[-Train,]

# Entreaamiento del modelo por medio de validacion cruzada

particiones<- 10

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "cv", number = particiones,
                              returnResamp = "all", verboseIter = FALSE,
)

# AJUSTE DEL MODELO REGRESION LINEAL
# ==============================================================================
modelo_LR <- train(ACPM~PERIODO, data = Entrenamiento,
                   method = "lm",
                   metric = "RMSE",
                   trControl = control_train)

modelo_LR
modelo_LR$finalModel
# PREDICCIONES MODELO REGRESION LINEAL
# ==============================================================================
predicciones_LR <- predict(modelo_LR, newdata = Prueba)
RMSE(predicciones_LR,Prueba$ACPM)


# AJUSTE DEL MODELO SVM KERNEL LINEAL
# ==============================================================================
modelo_svm_lineal <- train(ACPM~PERIODO, data = Entrenamiento,
                           method = "svmLinear",
                           metric = "RMSE",
                           trControl = control_train)

modelo_svm_lineal 

# PREDICCIONES MODELO SVM KERNEL LINEAL
# ==============================================================================
predicciones_svm_lineal <- predict(modelo_svm_lineal , newdata = Prueba)
RMSE(predicciones_svm_lineal,Prueba$ACPM)


# AJUSTE DEL MODELO SVM KERNEL POLINOMICO
# ==============================================================================
modelo_svm_poly <- train(ACPM~PERIODO, data = Entrenamiento,
                         method = "svmPoly",
                         metric = "RMSE",
                         trControl = control_train)

modelo_svm_poly 

# PREDICCIONES MODELO SVM KERNEL LINEAL
# ==============================================================================
predicciones_svm_poly <- predict(modelo_svm_poly , newdata = Prueba)
RMSE(predicciones_svm_poly,Prueba$ACPM)


# AJUSTE DEL MODELO SVM KERNEL RADIAL
# ==============================================================================
modelo_svm_rbf <- train(ACPM~PERIODO, data = Entrenamiento,
                        method = "svmRadial",
                        metric = "RMSE",
                        trControl = control_train)

modelo_svm_rbf 

# PREDICCIONES MODELO SVM KERNEL LINEAL
# ==============================================================================
predicciones_svm_rbf <- predict(modelo_svm_rbf  , newdata = Prueba)
RMSE(predicciones_svm_rbf,Prueba$ACPM)


# GRAFICAS FASE DE PRUEBA PARA LOS METODOS

# REGRESION LINEAL

Ameris <- as.data.frame(cbind(predicciones_LR,Prueba$ACPM))
plot(1:length(Prueba$PERIODO), Prueba$ACPM, 
     col="red", type="l", 
     main= "Gráfico Predicción Con Regresion Lineal Ventas ACPM",
     xlab = "Periodo",
     ylab= " Galones ACPM")
points(Ameris$predicciones_LR, col="green", type = "l")



# MODELO SVM KERNEL LINEAL

Ameris_Svm_L <- as.data.frame(cbind(predicciones_svm_lineal,Prueba$ACPM))
plot(1:length(Prueba$PERIODO), Prueba$ACPM, 
     col="red", type="l", 
     main= "Gráfico Predicción Con SVM Kernel Lineal Ventas ACPM",
     xlab = "Periodo",
     ylab= " Galones ACPM")
points(Ameris_Svm_L$predicciones_svm_lineal, col="green", type = "l")



# MODELO SVM KERNEL POLINOMICO

Ameris_Svm_Poly <- as.data.frame(cbind(predicciones_svm_poly,Prueba$ACPM))
plot(1:length(Prueba$PERIODO), Prueba$ACPM, 
     col="red", type="l", 
     main= "Gráfico Predicción Con SVM Kernel Polinomico Ventas ACPM",
     xlab = "Periodo",
     ylab= " Galones ACPM")
points(Ameris_Svm_Poly$predicciones_svm_poly, col="green", type = "l")


# MODELO SVM KERNEL RADIAL

Ameris_Svm_rbf <- as.data.frame(cbind(predicciones_svm_rbf,Prueba$ACPM))
plot(1:length(Prueba$PERIODO), Prueba$ACPM, 
     col="red", type="l", 
     main= "Gráfico Predicción Con SVM Kernel Radial Ventas ACPM",
     xlab = "Periodo",
     ylab= " Galones ACPM")
points(Ameris_Svm_rbf$predicciones_svm_rbf, col="green", type = "l")
# ==============================================================================
# COMPARACION DE METODOS TRADICIONLES VS METODOS MACHINE LEARNING
Datos_acpm_Compa <-read.table("Ventas_Acpm.txt", header = TRUE)
PERIODO <-c(121:132)
Pronostico_SVMRBF <- predict(modelo_svm_rbf, newdata= PERIODO)
Pronostico_SVMRBF 
RMSE(Pronostico_SVMRBF,Datos_acpm_Compa$ACPM)
