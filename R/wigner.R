#' @useDynLib randomMatRices
#' @importFrom Rcpp sourceCpp
#' 

rWigner <- function(n, dist = "normal", dDist = NULL, type = "real") {
  
  dDist <- ifelse(!is.null(dDist), dDist, dist)
  
  if (type == "real") {
    M <- realWigner(n, dist, dDist)
  } else if (type == "complex") {
    M <- complexWigner(n, dist, dDist)
  } else {
    stop("type must be either \"real\" or \"complex\"")
  }
  
  return(M)
  
}

realWigner <- function(n, dist, dDist) {
  
  if (dist == "normal") {
    v <- stats::rnorm(n*(n+1)/2, mean = 0, sd = 1)
    M <- symmetricMatrix(n, v)
  } else if (dist == "uniform") {
    l <- sqrt(3)
    v <- stats::runif(n*(n+1)/2, min = -l, max = l)
    M <- symmetricMatrix(n, v)
  } else {
    stop("Unknown distribution.")
  }
  
  if (dDist != dist) {
    if (dDist == "normal") {
      diag(M) <- stats::rnorm(n, mean = 0, sd = 1)
    } else if (dDist == "uniform") {
      l <- sqrt(3)
      diag(M) <- stats::runif(n, min = -l, max = l)
    } else {
      stop("Unknown distribution.")
    }  
  }

  return(M)
  
}

complexWigner <- function(n, dist, dDist) {
  
  if (dist == "normal") {
    rM <- symmetricMatrix(n, stats::rnorm(n*(n+1)/2, mean = 0, sd = 1))
    iM <- hermitianMatrix(n, stats::rnorm(n*(n+1)/2, mean = 0, sd = 1))
    d <- stats::rnorm(n, mean = 0, sd = 1)
  } else if (dist == "uniform") {
    l <- sqrt(3)
    rM <- symmetricMatrix(n, stats::runif(n*(n+1)/2, min = -l, max = l))
    iM <- hermitianMatrix(n, stats::runif(n*(n+1)/2, min = -l, max = l))
    d <- stats::runif(n, min = -l, max = l)
  } else {
    stop("Unknown distribution.")
  }
  
  M <- matrix(complex(real = rM, imaginary = iM), nrow = n, ncol = n)
  diag(M) <- d
  
  if (dDist != dist) {
    if (dDist == "normal") {
      diag(M) <- stats::rnorm(n, mean = 0, sd = 1)
    } else if (dDist == "uniform") {
      l <- sqrt(3)
      diag(M) <- stats::runif(n, min = -l, max = l)
    } else {
      stop("Unknown distribution.")
    }  
  }
  
  return(M)
    
}
