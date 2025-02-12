# Function for WITHOUT feedback learner  that updates the belief states over the
# probabilities of a source given the two stimuli X_I and X_F as well as the
# game_attributes and learner_type lists containing information about the
# present game and learner
update_source_belief <- function(prior_matrix, 
                                 matrix_to_forget_towards = NULL,
                                 X_I, 
                                 X_F, 
                                 game_attributes,
                                 resolution = 50,
                                 learner_type){
  
  # Set up probability vector used for numerical updating
  probability_vector <- seq(0.001, 1 - 0.001, length.out = resolution)
  
  # Vector and Matrices of likelihoods for the X_F
  likelihoods_X_F_vector <- dbinom(x = X_F,
                                   size = game_attributes$n_draws,
                                   prob = probability_vector)
  
  likelihoods_b_F_m <- array(likelihoods_X_F_vector,
                             dim = c(resolution,
                                     resolution))
  
  likelihoods_g_F_m <- t(array(rev(likelihoods_X_F_vector),
                               dim = c(resolution,
                                       resolution)))
  
  # Likelihoods for the initial draw X_I, and their mix
  likelihood_b_I <- dbinom(x = X_I,
                           size = game_attributes$n_draws,
                           prob = learner_type$b_I_subjective)
  
  likelihood_g_I <- dbinom(x = X_I,
                           size = game_attributes$n_draws,
                           prob = 1 - learner_type$g_I_subjective)
  
  mixed_likelihoods_m <- (likelihood_b_I * likelihoods_b_F_m * (1) + 
                            likelihood_g_I * likelihoods_g_F_m * (1))
  
  # Actual update (including re-normalization)
  updated_distribution_b_F_g_F <- sum_to_1(prior_matrix * mixed_likelihoods_m)
  
  # Only using the marginal distributions further ("Variational learner")
  if (learner_type$variational) {
    marginal_density_b_F <- rowSums(updated_distribution_b_F_g_F)
    marginal_density_g_F <- colSums(updated_distribution_b_F_g_F)
    updated_distribution_b_F_g_F <- outer(marginal_density_b_F,
                                          marginal_density_g_F)
  }
  
  # Forgetting / Move towards a given bias
  if (learner_type$lambda_forgetting != 0) {
    
    updated_distribution_b_F_g_F <- sum_to_1(
      (1 - learner_type$lambda_forgetting) * updated_distribution_b_F_g_F + 
        learner_type$lambda_forgetting * matrix_to_forget_towards)
    # The latter expression gets a uniform distribution matrix
  }
  
  return(updated_distribution_b_F_g_F)
}
