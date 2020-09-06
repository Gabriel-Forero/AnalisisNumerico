methods.bisection <- function(f, a, b, tol, n = 1000) {
    # Esta es solo una adaptacion del algoritmo entregado por la profesora
    # No se comenta por esta razon
    instancia.iteracion <- c()
    instancia.a <- c()
    instancia.b <- c()
    instancia.x <- c()
    instancia.error <- c()
    x <- seq(a, b, 0.01)
    x <- (a + b) / 2
    for (i in 1:n) {
        error <- abs(a - b) / 2
        instancia.iteracion <- c(instancia.iteracion, i)
        instancia.a <- c(instancia.a, asNumeric(a))
        instancia.b <- c(instancia.b, asNumeric(b))
        instancia.x <- c(instancia.x, asNumeric(x))
        instancia.error <- c(instancia.error, asNumeric(error))
        if (error >= tol) {
            if (f(x) * f(a) < 0) {
                b <- x
            } else {
                a <- x
            }
            x <- (a + b) / 2
        } else {
            instancia.data <- data.frame(
                "iteracion" = instancia.iteracion,
                "a" = instancia.a,
                "b" = instancia.b,
                "x" = instancia.x,
                "error" = instancia.error
            )
            return(list(
                encontrado = TRUE,
                cero = formatMpfr(x),
                error = formatMpfr(error),
                iteraciones = i,
                data = instancia.data
            ))
        }
    }
}


methods.secant <- function(f, x0, x1, tol, n = 1000) {
    # Inicializamos vectores de informacion
    instancia.iteracion <- c()
    instancia.x0 <- c()
    instancia.x1 <- c()
    instancia.x2 <- c()
    instancia.f0 <- c()
    instancia.f1 <- c()
    instancia.f2 <- c()
    instancia.error <- c()
    # Llevamos un contador con las iteraciones realizadas
    k <- 1
    # Hacemos las iteraciones necesarias no mas que n
    for (i in 1:n) {
        # Sacamos la imagen de los valores iniciales
        f0 <- f(x0)
        f1 <- f(x1)
        # Calculamos el punto de corte con el eje X
        c <- x1 - f1 * (x1 - x0) / (f1 - f0)
        # Si nos encontramos con un valor sin sentido salimos
        if (is.nan(c) | c == Inf | c == -Inf) {
            break
        }
        # Para llevar la informacion completa calculamos
        x2 <- c
        f2 <- f(x2)
        error <- abs(x2 - x1)
        # If tenemos un valor con la tolerancia deseada lo retornamos
        if (error < tol) {
            # Condensamos la informacion en un dataframe para su analisis
            instancia.data <- data.frame(
                "iteracion" = instancia.iteracion,
                "x0" = instancia.x0,
                "x1" = instancia.x1,
                "x2" = instancia.x2,
                "f0" = instancia.f0,
                "f1" = instancia.f1,
                "f2" = instancia.f2,
                "error" = instancia.error
            )
            # Devolvemos una lista para facilitar el acceso rapido a los resultados
            return(list(
                encontrado = TRUE,
                cero = formatMpfr(x2),
                fcero = formatMpfr(f2),
                error = formatMpfr(error),
                iteraciones = k,
                data = instancia.data
            ))
        }
        # Anadimos una fila a nuestro dataframe para registrar la iteracion
        instancia.iteracion <- c(instancia.iteracion, k)
        instancia.x0 <- c(instancia.x0, asNumeric(x0))
        instancia.x1 <- c(instancia.x1, asNumeric(x1))
        instancia.x2 <- c(instancia.x2, asNumeric(x2))
        instancia.f0 <- c(instancia.f0, asNumeric(f0))
        instancia.f1 <- c(instancia.f1, asNumeric(f1))
        instancia.f2 <- c(instancia.f2, asNumeric(f2))
        instancia.error <- c(instancia.error, asNumeric(error))
        # Avanzamos alimentando la siguiente iteracion
        x0 <- x1
        x1 <- x2
        k <- k + 1
    }
    # Si llegamos a este punto el ciclo finalizo o se rompio
    # retornamos la mejor aproximacion antes de haber terminado
    instancia.data <- data.frame(
        "iteracion" = instancia.iteracion,
        "x0" = instancia.x0,
        "x1" = instancia.x1,
        "x2" = instancia.x2,
        "f0" = instancia.f0,
        "f1" = instancia.f1,
        "f2" = instancia.f2,
        "error" = instancia.error
    )
    return(list(
        encontrado = FALSE,
        cero = formatMpfr(x2),
        fcero = formatMpfr(f2),
        error = formatMpfr(error),
        iteraciones = k,
        data = instancia.data
    ))
}