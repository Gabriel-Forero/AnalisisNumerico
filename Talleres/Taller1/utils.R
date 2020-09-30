toleranciaFactory <- function(toleranciaDeseada) {
  # Bits de precision que calculamos para la tolerancia eg: -8, -16 ...
  precisionBits <- ceiling(abs(toleranciaDeseada) / (log(2, base = 10)))
  # 1e^toleranciaDeseada con precision deseada
  tol <- mpfr(1 * 10^toleranciaDeseada, precisionBits)
  return(list(tol = tol, bits = precisionBits, magnitud = toleranciaDeseada))
}