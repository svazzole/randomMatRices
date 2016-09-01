rGOE <- function(n) {
  
  M <- rWigner(n)
  M <- (M + t(M))/sqrt(2)
  return(M)
  
}