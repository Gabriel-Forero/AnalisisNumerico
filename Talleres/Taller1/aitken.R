library("Rmpfr")
library("ggplot2")
library("reshape2")
setwd("C:/Users/jhvan/OneDrive - Pontificia Universidad Javeriana/2020-03/Analisis Numerico/Talleres/Taller 1/codigo")
source("./methods.R")
source("./utils.R")

#Aitken
fa <- function(x) x^2-cos(x)
df <- function(x) 2*x + sin(x)
#Aqui cambiamos el tipo de tolerancia que necesita la prueba
tol <- toleranciaFactory(-16)
resA <- methods.newton_aitken(fa, df, mpfr(1, tol$bits), tol$tol)
print(resA)

#Steffensen
fx <- function(x) sqrt(cos(x))
resF <- methods.steffensen(f, fx, mpfr(1, tol$bits), tol$tol)
print(resF)

plot(resA$iteraciones, resA$values, type = "o", xlab = "x", ylab = "f(x)")
title(main = "Convergencia Aitken")
plot(resF$iteraciones, resF$values, type = "o", xlab = "x", ylab = "f(x)")
title(main = "Convergencia Steffensen")