#---------------------------------------------------------------------------------
# Librerias necesarias
library(dplyr)
library(rriskDistributions)
library(normtest)
#---------------------------------------------------------------------------------

# Limpiamos memoria
rm(list=ls())

# Fijamos una semilla
set.seed(1006)

# Cargamos la data 
Ventas <- read.table("Ventas.txt", header = TRUE)

Corriente <- Ventas$CORRIENTE
Extra <- Ventas$EXTRA
Acpm <- Ventas$ACPM
hist(Corriente, main= " Histograma Corriente")
hist(Extra, main= " Histograma Extra")
hist(Acpm, main= " Histograma Acpm")

# Prueba De Normalidad
lillie.test(Corriente)
lillie.test(Extra)
lillie.test(Acpm)

