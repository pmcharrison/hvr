#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
double cosine_similarity(const NumericVector &x,
                         const NumericVector &y) {
  return sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)));
}

// [[Rcpp::export]]
NumericVector cosine_similarities(const NumericVector &vector,
                                  const NumericMatrix &vectors) {
  unsigned n = vectors.nrow();
  NumericVector res(n);
  for (int i = 0; i < n; i ++) {
    res[i] = cosine_similarity(vector,
                               static_cast<NumericVector>(vectors.row(i)));
  }
  return res;
}
