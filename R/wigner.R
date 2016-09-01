rWigner <- function(n, dist = "normal", dDist = "normal", type = "real") {
  
  if (type == "real") {
    M <- realWigner(n, dist, dDist)
  } else if (type == "complex") {
    M <- realWigner(n, dist, dDist)
  } else {
    stop("type must be either \"real\" or \"complex\"")
  }
  
  return(M)
  
}

realWigner <- function(n, dist, dDist) {
  
  if (dist == "normal") {
    m <- stats::rnorm(n^2, mean = 0, sd = 1)
    M <- matrix(m, nrow = n, ncol = n)
    M <- 1/sqrt(2) * (M + t(M))
  } else if (dist == "uniform") {
    l <- sqrt(3)
    m <- stats::runif(n^2, min = -l, max = l)
    M <- matrix(m, nrow = n, ncol = n)  
    M <- setLowerTriangToZero(M)
    M <- M + t(M) + diag(x = stats::runif(n, min = -l, max = l), nrow = n, ncol = n)
  } else {
    stop("Unknown distribution.")
  }
  
  return(M)
  
}

setLowerTriangToZero <- function(M) {
  
  nr <- nrow(M)
  nc <- ncol(M)
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      if (i <= j) {
        M[i,j] <- 0
      }
    }
  }
  
  return(M)
  
}
