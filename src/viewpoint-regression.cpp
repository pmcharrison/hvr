#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
double cost (const NumericVector &weights,
             const NumericMatrix &observation_matrix,
             const List &continuation_matrices,
             const List &legal) {
  int num_weights = weights.size();
  int num_seq = continuation_matrices.size();
  int num_obs = observation_matrix.nrow();

  double corpus_cost = 0.0;

  for (int i = 0; i < num_seq; i ++) { // over events
    const NumericMatrix &continuation_matrix = continuation_matrices[i];
    const LogicalVector &legal_i = legal[i];
    if (legal_i.size() != continuation_matrix.nrow())
      stop("'legal' and 'continuation matrix' did not conform");

    int alphabet_size = continuation_matrix.nrow();

    double observation_energy = 0.0;
    for (int k = 0; k < num_weights; k ++) {
      observation_energy += observation_matrix(i, k) * weights[k];
    }

    double z = 0.0;
    for (int j = 0; j < alphabet_size; j ++) { // over potential symbols
      if (legal_i[j]) {
        double energy = 0.0;
        for (int k = 0; k < num_weights; k ++)  { // over features
          energy += continuation_matrix(j, k) * weights[k];
        }
        z += exp(energy);
      }
    }

    // Rcout << "alphabet_size = " << alphabet_size << "\n";
    // Rcout << "observation_energy = " << observation_energy << "\n";
    double observation_probability = exp(observation_energy) / z;
    // Rcout << "observation_probability = " << observation_probability << "\n";
    corpus_cost -= log(observation_probability);
  }
  double event_cost_nats = corpus_cost / num_obs;
  double event_cost_bits = event_cost_nats * log2(exp(1.0));
  return event_cost_bits;
}

// [[Rcpp::export]]
NumericVector gradient (const NumericVector &weights,
                        const NumericMatrix &observation_matrix,
                        const List &continuation_matrices,
                        const List &legal) {
  int num_weights = weights.size();
  int num_seq = continuation_matrices.size();
  int num_obs = observation_matrix.nrow();

  NumericVector corpus_grad(num_weights, 0.0);

  for (int i = 0; i < num_seq; i ++) { // over events
    const NumericMatrix &continuation_matrix = continuation_matrices[i];
    const LogicalVector &legal_i = legal[i];

    if (legal_i.size() != continuation_matrix.nrow())
      stop("'legal' and 'continuation matrix' did not conform");

    int alphabet_size = continuation_matrix.nrow();
    double z = 0.0;
    NumericVector z_prime(num_weights, 0.0);

    for (int j = 0; j < alphabet_size; j ++) { // over potential symbols
      if (legal_i[j]) {
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
    }
    for (int k = 0; k < num_weights; k ++) { // one more time over features
      corpus_grad[k] += z_prime[k] / z - observation_matrix(i, k);
    }
  }
  NumericVector event_grad_bits = corpus_grad * (log2(exp(1.0)) / num_obs);
  return event_grad_bits;
}
