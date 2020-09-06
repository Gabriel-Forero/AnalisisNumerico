ruta.base <- "./"
ruta.imagenes <- file.path(ruta.base, "imagenes")
dir.create(ruta.imagenes, showWarnings = FALSE)
ruta.data <- file.path(ruta.base, "data")
dir.create(ruta.data, showWarnings = FALSE)

p.graficarMetodo <- function(experimento) {
  # Definimos y creamos las rutas para los archivos
  ruta.generales <- file.path(ruta.imagenes, "generales")
  dir.create(ruta.generales, showWarnings = FALSE)
  ruta.metodo <- file.path(ruta.generales, experimento$metodo)
  dir.create(ruta.metodo, showWarnings = FALSE)
  ruta.fn <- file.path(ruta.metodo, experimento$fn)
  dir.create(ruta.fn, showWarnings = FALSE)
  ruta.magnitud <- file.path(ruta.fn, experimento$magnitud)
  dir.create(ruta.magnitud, showWarnings = FALSE)
  # Exponemos los datos por conveniencia
  datos <- experimento$data
  # Nombramos el archivo
  archivo.convergencia <- file.path(ruta.magnitud, paste(experimento$metodo, "_", abs(experimento$magnitud), "_convergencia.png", sep = ""))
  archivo.error_absoluto <- file.path(ruta.magnitud, paste(experimento$metodo, "_", abs(experimento$magnitud), "_error_absoluto.png", sep = ""))
  # Generamos los archivos
  png(archivo.convergencia, width = 700, height = 700)
  if(experimento$metodo == "secante"){
    plot(data.frame(datos["iteracion"], datos["x2"]), xlab = "Iteraciones", ylab = "Raiz", main = paste("Orden de convergencia", experimento$fex, ", magnitud", experimento$magnitud), type = "b")
  }else{
    plot(data.frame(datos["iteracion"], datos["x"]), xlab = "Iteraciones", ylab = "Raiz", main = paste("Orden de convergencia", experimento$fex, ", magnitud", experimento$magnitud), type = "b")
  }
  mtext(paste("Raiz encontrada ", experimento$cero, sep = ""), side = 3)
  title(paste("Metodo", experimento$metodo), line = -1, outer = TRUE)
  dev.off()
  png(archivo.error_absoluto, width = 700, height = 700)
  plot(data.frame(datos["iteracion"], datos["error"]), xlab = "Iteraciones", ylab = "Error absoluto", main = paste("Error absoluto", experimento$fex, ", magnitud", experimento$magnitud), type = "b", col = "red")
  title(paste("Metodo", experimento$metodo), line = -1, outer = TRUE)
  dev.off()
}

p.graficarAnimacionSecante <- function(experimento) {
  # Separamos los datos por conveniencia
  datos <- experimento$data
  fx <- experimento$f
  # Recorremos las filas del dataframe
  for (i in 1:nrow(datos)) {
    # Definimos y creamos las rutas para los archivos
    ruta.animacion <- file.path(ruta.imagenes, "animacion_secante")
    dir.create(ruta.animacion, showWarnings = FALSE)
    ruta.fn <- file.path(ruta.animacion, experimento$fn)
    dir.create(ruta.fn, showWarnings = FALSE)
    ruta.magnitud <- file.path(ruta.fn, experimento$magnitud)
    dir.create(ruta.magnitud, showWarnings = FALSE)
    ins <- datos[i, ]
    archivo <- file.path(ruta.magnitud, paste(ins$iteracion, ".png", sep = ""))
    png(archivo, width = 700, height = 700)
    # Desplegamos los datos por conveniencia
    x0 <- ins$x0
    x1 <- ins$x1
    x2 <- ins$x2
    f0 <- ins$f0
    f1 <- ins$f1
    f2 <- ins$f2
    # Calculamos los margenes de la grafica
    margenError <- 0.1
    xlimits <- c(min(x0, x1, x2) - margenError, max(x0, x1, x2) + margenError)
    ylimits <- c(min(f0, f1, f2) - margenError, max(f0, f1, f2) + margenError)
    # Graficamos la funcion
    curve(fx, xlim = xlimits, ylim = ylimits, col = "blue", lwd = 1.5, lty = 2)
    # Le ponemos un titulo
    title(main = paste(experimento$fex, ", k = ", ins$iteracion))
    # Graficamos los ejes
    abline(h = 0)
    abline(v = 0)
    # Graficamos los puntos de interes
    points(ins$x0, ins$f0, cex = 1.25, pch = 21, bg = "blue", col = "blue")
    points(ins$x1, ins$f1, cex = 1.25, pch = 21, bg = "blue", col = "blue")
    points(ins$x2, 0, cex = 1.25, pch = 21, bg = "blue", col = "green")
    # Calculamos la pendiente y la abscisa de la secante
    pendiente <- (f1 - f0) / (x1 - x0)
    if (is.na(pendiente) | pendiente == -Inf | pendiente == Inf) {
      pendiente <- 0
    }
    intercepto <- f1 - pendiente * x1
    # Graficamos la secante
    abline(a = intercepto, b = pendiente, col = "red")
    # Le ponemos la leyenda
    legend("top",
      legend = c(experimento$fex, "Secante"),
      col = c("blue", "red"), lty = 1:2, cex = 0.8,
      title = "Leyenda", text.font = 4, bg = "lightblue"
    )
    # Cerramos el archivo
    dev.off()
  }
}

p.exportarExcel <- function(experimento) {
  ruta.metodo <- file.path(ruta.data, experimento$metodo)
  dir.create(ruta.metodo, showWarnings = FALSE)
  ruta <- file.path(ruta.metodo, "data.xlsx")
  nombreSheet <- paste(experimento$fn, experimento$magnitud, sep = "")
  # Escribimos al excel en la hoja definida el dataframe correspondiente
  write.xlsx(experimento$data, file = ruta, sheetName = nombreSheet, append = TRUE)
}

p.graficarConvergenciaComparativa <- function(comparacion, y.max, fn, fex) {
  # Creamos el directorio
  ruta.convergencia <- file.path(ruta.imagenes, "convergencia")
  dir.create(ruta.convergencia, showWarnings = FALSE)
  # Creamos el archivo
  archivo <- file.path(ruta.convergencia, paste(fn, "comparativa.png", sep = "_"))
  png(archivo, width = 700, height = 700)
  #Graficamos
  plot(comparacion$magnitud, comparacion$secante, type = "o", xlab = "Magnitud", ylab = "Iteraciones", main = paste("Orden de convergencia", fex), ylim = c(0, y.max))
  lines(comparacion$magnitud, comparacion$biyeccion, type = "o", col = "red")
  # Guardamos el archivo
  dev.off()
}