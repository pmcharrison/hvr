#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
double cost (const NumericVector &weights,
             const NumericMatrix &observation_matrix,
             const List &continuation_matrices) {
  int num_weights = weights.size();
  int num_seq = continuation_matrices.size();

  double cost = 0.0;

  for (int i = 0; i < num_seq; i ++) { // over events
    const NumericMatrix &continuation_matrix = continuation_matrices[i];
    int alphabet_size = continuation_matrix.nrow();

    double observation_energy = 0.0;
    for (int k = 0; k < num_weights; k ++) {
      observation_energy += observation_matrix(i, k) * weights[k];
    }

    double z = 0.0;
    for (int j = 0; j < alphabet_size; j ++) { // over potential symbols
      double energy = 0.0;
      for (int k = 0; k < num_weights; k ++)  { // over features
        energy += continuation_matrix(j, k) * weights[k];
      }
      // Rcout << "j = " << j << "\n";
      // Rcout << "energy = " << energy << "\n";
      // Rcout << "exp_energy = " << exp(energy) << "\n";
      z += exp(energy);
      // Rcout << "new z = " << z << "\n\n";
    }

    // Rcout << "alphabet_size = " << alphabet_size << "\n";
    // Rcout << "observation_energy = " << observation_energy << "\n";
    double observation_probability = exp(observation_energy) / z;
    // Rcout << "observation_probability = " << observation_probability << "\n";
    cost -= log(observation_probability);
  }
  return cost;
}

// [[Rcpp::export]]
NumericVector gradient (const NumericVector &weights,
                        const NumericMatrix &observation_matrix,
                        const List &continuation_matrices) {
  int num_weights = weights.size();
  int num_seq = continuation_matrices.size();

  NumericVector grad(num_weights, 0.0);

  for (int i = 0; i < num_seq; i ++) { // over events
    const NumericMatrix &continuation_matrix = continuation_matrices[i];
    int alphabet_size = continuation_matrix.nrow();
    double z = 0.0;
    NumericVector z_prime(num_weights, 0.0);
    for (int j = 0; j < alphabet_size; j ++) { // over potential symbols
      double energy = 0.0;
      for (int k = 0; k < num_weights; k ++)  { // over features
        energy += continuation_matrix(j, k) * weights[k];
      }
      double exp_energy = exp(energy);
      z += exp_energy;
      for (int k = 0; k < num_weights; k ++) { // over features again
        z_prime[k] += continuation_matrix(j, k) * exp_energy;
      }
    }
    for (int k = 0; k < num_weights; k ++) { // one more time over features
      grad[k] += z_prime[k] / z - observation_matrix(i, k);
    }
  }
  return grad;
}
