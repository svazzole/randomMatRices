rGOE <- function(n) {
  
  M <- rWigner(n)
  M <- (M + t(M))/sqrt(2)
  return(M)
  
}

rSparseMatrix <- function(n, nz = 0.1) {
  
  ix <- sample(1:n, size = floor(nz*(n^2)), replace = TRUE)
  jx <- sample(1:n, size = floor(nz*(n^2)), replace = TRUE)
  M <- Matrix::sparseMatrix(i = ix, j = jx, x = stats::rnorm(floor(nz*(n^2))))
  
  return(M)
}

rSymSparseMatrix <- function(n, nz = 0.1) {
  
  ix <- sample(1:n, size = floor(nz*(n^2)), replace = TRUE)
  jx <- sample(1:n, size = floor(nz*(n^2)), replace = TRUE)
  M <- Matrix::sparseMatrix(i = ix, j = jx, x = stats::rnorm(floor(nz*(n^2))))
  M <- (M + Matrix::t(M))/sqrt(2)
  return(M)
}