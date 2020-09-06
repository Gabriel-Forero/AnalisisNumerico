# Recordar primero instalar las librerias necesarias
library("Rmpfr")
library("xlsx")
# En caso de que haya errores en la localizacion del codigo ingresar la ruta absoluta del codigo
# por ejemplo: "C:/Users/jhvan/OneDrive - Pontificia Universidad Javeriana/2020-03/Analisis Numerico/Taller"
setwd("C:/Users/jhvan/OneDrive - Pontificia Universidad Javeriana/2020-03/Analisis Numerico/AnalisisNumerico/Parciales/Parcial 1/Exposicion - Metodo secante/codigo")
source("./methods.R")
source("./presentation.R")
# Tolerancia calculada por 10^toleranciaDeseada, formula obtenida de la documentacion
# de Rmpfr https://cran.r-project.org/web/packages/Rmpfr/vignettes/Rmpfr-pkg.pdf
# pagina 4
toleranciaFactory <- function(toleranciaDeseada) {
  # Bits de precision que calculamos para la tolerancia eg: -8, -16 ...
  precisionBits <- ceiling(abs(toleranciaDeseada) / (log(2, base = 10)))
  # 1e^toleranciaDeseada con precision deseada
  tol <- mpfr(1 * 10^toleranciaDeseada, precisionBits)
  return(list(tol = tol, bits = precisionBits, magnitud = toleranciaDeseada))
}

procesarResultado <- function(resultado) {
  # Presentamos los resultados generales al usuario
  cat("tol -> ", resultado$magnitud, ", f(x) -> ", resultado$fex, ", uniroot: ", resultado$uniroot, "\n")
  # Por cada metodo en los resultados
  for (metodo in resultado$metodos) {
    # Presentamos los resultados de este metodo
    cat("\t", metodo$metodo, " -> k = ", metodo$iteraciones, ", x = ", metodo$cero, ", error = ", metodo$error, "\n")
    # Graficamos en un arhivo los resultados del metodo
    p.graficarMetodo(metodo)
    #p.exportarExcel(metodo)
  }
  p.graficarAnimacionSecante(resultado$metodos$secante)
}

generador <- function(experimentos, tolerancias) {
  for (e in experimentos) {
    vector.m <- c()
    vector.s <- c()
    vector.b <- c()
    # Si el rango no esta bien configurado no procede
    if (e$rango[1] > e$rango[2]) {
      return("Error: el rango no es valido")
    }
    # Corroboramos la raiz con la funcion uniroot de R
    ceroReferencia <- uniroot(e$f, e$rango)
    for (t in tolerancias) {
      # Generamos los valores iniciales dentro del rango evaluado
      semillas <- runif(2, e$rango[1], e$rango[2])
      semillas <- mpfr(semillas, t$bits)
      rango <- mpfr(e$rango, t$bits)
      # Realizamos el experimento con el metodo secante
      resultadoS <- methods.secant(e$f, semillas[1], semillas[2], t$tol)
      resultadoS$metodo <- "secante"
      resultadoS$f <- e$f
      resultadoS$fn <- e$id
      resultadoS$fex <- e$fex
      resultadoS$magnitud <- t$magnitud
      resultadoS$rango <- e$rango
      resultadoS$uniroot <- ceroReferencia$root
      # Realizamos el experimento con el metodo biyeccion
      resultadoB <- methods.bisection(e$f, rango[1], rango[2], t$tol)
      resultadoB$metodo <- "biyeccion"
      resultadoB$f <- e$f
      resultadoB$fn <- e$id
      resultadoB$fex <- e$fex
      resultadoB$magnitud <- t$magnitud
      resultadoB$rango <- e$rango
      resultadoB$uniroot <- ceroReferencia$root
      # Calculamos la columna de errorAbsoluto contra la raiz de uniroot
      errorAbsoluto <- abs(ceroReferencia$root - resultadoS$data[, "x2"])
      # Calculamos la columna de errorRelativo contra la k-1
      errorRelativo <- abs(resultadoS$data[, "error"] / resultadoS$data[, "x1"])
      # Las anadimos al dataframe
      resultadoS$data$errorAbsoluto <- errorAbsoluto
      resultadoS$data$errorRelativo <- errorRelativo
      vector.m <- c(vector.m, abs(t$magnitud))
      vector.s <- c(vector.s, resultadoS$iteraciones)
      vector.b <- c(vector.b, resultadoB$iteraciones)
      resultadoExperimento <- list(
        metodos = list(
          secante = resultadoS,
          biyeccion = resultadoB
        ),
        f = e$f,
        fn = e$id,
        fex = e$fex,
        magnitud = t$magnitud,
        rango = e$rango,
        uniroot = ceroReferencia$root
      )
      procesarResultado(resultadoExperimento)
    }
    comparacion <- data.frame("magnitud" = vector.m, "secante" = vector.s, "biyeccion" = vector.b)
    y.max <- max(vector.b)
    p.graficarConvergenciaComparativa(comparacion, y.max, e$id, e$fex)
  }
}

performTest <- function() {
  # Definimos opciones para graficos
  par(pch = 19)
  par(bg = "white")
  par(mfrow = c(1, 1))
  options(digits=22)
  # Definimos las funciones a usar y los rangos de uso, tambien informacion util
  experimentos <- list(
    list(id = "F1", f = function(x) cos(2 * x)^2 - (x^2), rango = c(0, 3), fex = "cos(2 * x) ^ 2 - (x ^ 2)"),
    list(id = "F2", f = function(x) x * sin(x) - 1, rango = c(-1, 2), fex = "x * sin(x) - 1"),
    list(id = "F3", f = function(x) x^3 - (2 * x^2) + ((4 / 3) * x) - (8 / 27), rango = c(0, 2), fex = "x ^ 3 - (2 * x ^ 2) + ((4 / 3) * x) - (8 / 27)")
  )
  # Definimos las magnitudes para el experimento
  # magnitudes <- seq(-8, -32, -1)
  magnitudes <- c(-8, -16, -32)
  # Definimos las tolerancias
  tolerancias <- lapply(magnitudes, toleranciaFactory)
  # Iniciamos el generador para los datos de entrada de los metodos
  generador(experimentos, tolerancias)
}
# Corremos toda la simulacion
performTest()