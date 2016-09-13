#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix symmetricMatrix(int n, NumericVector v) {
  
  int k = 0;

  Rcpp::NumericMatrix M(n,n);
  
  for (int i=0; i<n; i++) {
    for (int j=i; j<n; j++) {
      M(i,j) = v(k);
      M(j,i) = v(k);
      k++;
    }
  }
  
  return Rcpp::wrap(M);
  
}

// [[Rcpp::export]]
NumericMatrix hermitianMatrix(int n, NumericVector v) {
  
  int k = 0;
  
  Rcpp::NumericMatrix M(n,n);
  
  for (int i=0; i<n; i++) {
    for (int j=i; j<n; j++) {
      M(i,j) = v(k);
      M(j,i) = -v(k);
      k++;
    }
  }
  
  return Rcpp::wrap(M);
  
}