
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hvr

<!-- badges: start -->

<!-- badges: end -->

We have some practical notes for researchers wishing to train the model
on new datasets. Model training comprises two steps: training the
categorical viewpoint models and optimizing the regression weights. The
categorical viewpoint models are data-hungry because they contain many
thousands of parameters, but they are fast to train: given a training
corpus, the algorithm simply computes feature values for each sequence
and saves all n-grams from these sequences for n \< 12. Optimizing
regression weights requires less training data because there are only
about 40 parameters to optimize, but training is relatively slow because
of an expensive normalization constant that needs to be computed over
the entire alphabet of 24,576 chords. Correspondingly, we recommend
using a large training corpus for training the categorical viewpoint
models, and a small corpus for optimizing viewpoint weights, where the
latter corpus is preferably constructed by downsampling from a larger
corpus. As a rule of thumb, we recommend that the viewpoint optimization
corpus should contain at least 5 times as many chords as the number of
regression weights. Future strategies for speeding up the weight
optimization stage could include stochastic gradient descent and
approximating the normalization constant.
