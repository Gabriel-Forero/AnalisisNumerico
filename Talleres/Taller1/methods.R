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

methods.horner <- function(coeficientes, x) {
    n <- length(coeficientes) - 1
    p <- coeficientes[1]
    d1 <- 0
    d2 <- 0
    multi.p <- 0
    multi.d1 <- 0
    multi.d2 <- 0
    for (i in 1:n) {
        d2 <- d2 * x + 2 * d1
        multi.d2 <- multi.d2 + 2
        d1 <- d1 * x + p
        multi.d1 <- multi.d1 + 1
        p <- p * x + coeficientes[i + 1]
        multi.p <- multi.p + 1
    }
    return(list(
        "p" = p,
        "d1" = d1,
        "d2" = d2,
        "multi.p" = multi.p,
        "multi.d1" = multi.p + multi.d1,
        "multi.d2" = multi.p + multi.d1 + multi.d2
    ))
}

methods.laguerre <- function(coeficientes, x, tolerancia) {
    multi <- 0
    i <- 0
    p <- 10000
    n <- length(coeficientes) - 1
    while (abs(p) > tolerancia) {
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        pD1 <- horner$d1
        pD2 <- horner$d2
        multi <- multi + horner$multi.d2
        G <- pD1 / p
        H <- G^2 - (pD2 / p)
        factorInterno <- (n - 1) * ((n * H) - G^2)
        raiz <- sqrt(as.complex(as.numeric(factorInterno)))
        x2 <- 0
        if (abs(G + raiz) > abs(G - raiz)) {
            x2 <- x - (n / (G + raiz))
        } else {
            x2 <- x - (n / (G - raiz))
        }
        x <- x2
        i <- i + 1
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        multi <- multi + horner$multi.p
    }
    return(list(
        "r" = x,
        "iter" = i,
        "multi" = multi
    ))
}

methods.newton_horner <- function(coeficientes, x, tolerancia) {
    i <- 0
    multi <- 0
    p <- 10000
    while (abs(p) > tolerancia) {
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        pD1 <- horner$d1
        multi <- multi + horner$multi.d1
        x2 <- x - (p / pD1)
        x <- x2
        horner <- methods.horner(coeficientes, x)
        p <- horner$p
        multi <- multi + horner$multi.d1
        i <- i + 1
    }
    return(list(
        "r" = x,
        "iter" = i,
        "multi" = multi
    ))
}

methods.aitken_sucesion <- function(x, x1, x2){
    num <- (x2-x1)^2
    den <- x2 - (2*x1) + x
    x3 <- x2 - (num/den)
    return(x3)
}

methods.newton_aitken <- function(f, df, x, tolerancia) {
    i <- 1
    p <- 10000
    j <- 1
    ax <- c(x)
    iter <- c(i)
    value <- c(x)
    while (abs(p) > tolerancia) {
        p <- f(x)
        pD1 <- df(x)
        x2 <- x - (p / pD1)
        x <- x2
        ax = c(ax, x)
        j <- j + 1
        if(j == 3){
            x <- methods.aitken_sucesion(ax[1], ax[2], ax[3])
            ax <- c(x)
            j <- 1
        }
        p <- f(x)
        i <- i + 1
        iter <- c(iter, i)
        value <- c(value, x)
    }
    return(list(
        "r" = x,
        "iter" = i,
        "iteraciones" = iter,
        "values" = value
    ))
}

methods.fixed_point <- function(f, fx, x, tolerancia){
    i <- 0
    p <- 10000
    while(abs(p) > tolerancia){
        x <- fx(x)
        p <- f(x)
        i <- i + 1
    }
    return(list(
        "r" = x,
        "iter" = i
    ))
}

methods.steffensen <- function(f, fx, x, tolerancia){
    i <- 1
    p <- 10000
    j <- 1
    sx <- c(x)
    iter <- c(i)
    value <- c(x)
    while(abs(p) > tolerancia){
        x <- fx(x)
        sx = c(sx, x)
        j <- j + 1
        if(j == 3){
            x <- methods.aitken_sucesion(sx[1], sx[2], sx[3])
            sx <- c(x)
            j <- 1
        }
        p <- f(x)
        i <- i + 1
        iter <- c(iter, i)
        value <- c(value, x)
    }
    return(list(
        "r" = x,
        "iter" = i,
        "iteraciones" = iter,
        "values" = value
    ))
}