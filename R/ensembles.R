rGOE <- function(n) {
  
  v <- stats::rnorm(n^2, mean = 0, sd = 1)
  M <- matrix(v, n, n)
  #M <- rWigner(n)
  M <- (M + t(M))/sqrt(2)
  return(M)
  
}

rGUE <- function(n) {
  
  M <- rWigner(n, type = "complex")
  M <- (M + t(Conj(M)))/sqrt(2)
  return(M)
  
}
