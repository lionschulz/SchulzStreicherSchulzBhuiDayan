# Function for one trial update of within-trial belief update.
update_within_trial_belief <- function(likelihood_b_I,
                                       likelihood_g_I,
                                       X_F,
                                       marginal_density_b_F,
                                       marginal_density_g_F,
                                       game_attributes,
                                       resolution = 50) {
  
  probabilities_v <- seq(0, 1, length.out = resolution)
  
  likelihoods_b_F <- dbinom(X_F, 
                            game_attributes$n_draws, 
                            probabilities_v)
  likelihoods_g_F <- dbinom(game_attributes$n_draws - X_F, 
                            game_attributes$n_draws, 
                            probabilities_v)
  
  # print(paste("likelihood g_F:", likelihoods_g_F))
  # print(paste("marginal density g_F:", marginal_density_g_F))
  
  integral_b_F <- sum(likelihoods_b_F * marginal_density_b_F)
  integral_g_F <- sum(likelihoods_g_F * marginal_density_g_F)
  
  # print(paste("Integral b_F:", integral_b_F))
  # print(paste("Integral g_F:", integral_g_F))
  
  posterior_blue <- likelihood_b_I*integral_b_F /
                    (likelihood_b_I*integral_b_F + likelihood_g_I*integral_g_F)
  
  # print(paste("Postererior Blue is:", posterior_blue))
  
  return(posterior_blue)
}

  


run_within_trial_belief_update <- function(
    X_I_v, 
    X_F_v, 
    learner_type, 
    learner_output, 
    game_attributes,
    resolution = 50){
  # Function that runs the within trial update on a vector of random variables
  # X_I_v and X_F_v, given 
  # - learner_type (list containing e.g. priors) 
  # - learner_output (list containing the marginal densities over the belief states)
  # - game_attributes (list containing the attributes of the currently played game)
  # - resolution of numerical integration
  # Returns a vector of posteriors that blue is better
  
  
  # First, we compute the likelihoods based on the X_I vector
  if(game_attributes$news_station_first == FALSE){
    likelihood_b_I_v <- dbinom(X_I_v, 
                               game_attributes$n_draws, 
                               game_attributes$b_I)
    likelihood_g_I_v <- dbinom(game_attributes$n_draws - X_I_v, 
                               game_attributes$n_draws,
                               game_attributes$g_I)
  } else { # Use a flat prior
    # Also applies to the condition without the news station council
    likelihood_b_I_v <- likelihood_g_I_v <- rep(0.5, game_attributes$n_trials)
  }

  
  # print(paste("Likelihood b_I:", likelihood_b_I))
  # print(paste("Likelihood g_I:", likelihood_g_I))
  
  # When we do the WITH feedback learning, we need to create the marginal
  # distributions first to be able to integrate over 
  # (in the case of the WITHOUT feedback learner this is provided by the learner_type already)
  if (learner_type$type == "ground_truth") {
    
    learner_output <- 
      get_marginal_densitities_for_ground_truth_learner(
        learner_output,
        game_attributes,
        resolution = resolution
      )
    
  }
  
  # Run the actual update
  posterior_blue_v <- 1:game_attributes$n_trials
  
  for (t in 1:game_attributes$n_trials) {
    
    posterior_blue_v[t] <- 
      update_within_trial_belief(likelihood_b_I = likelihood_b_I_v[t],
                                 likelihood_g_I = likelihood_g_I_v[t],
                                 X_F = X_F_v[t],
                                 marginal_density_b_F = 
                                   learner_output$marginal_density_b_F_l[[t]],
                                 marginal_density_g_F = 
                                   learner_output$marginal_density_g_F_l[[t]],
                                 game_attributes = game_attributes,
                                 resolution = resolution)
                         
  }
  
  return(posterior_blue_v)
}

# Gets the 'marginal densities' of the belief states of the ground truth learner
# This needs to be done because the ground truth learner only operates based
# on alpha and beta of the beta distribution
# Processes the learner output to add the marginal densitities
get_marginal_densitities_for_ground_truth_learner <- function(
    learner_output,
    game_attributes,
    resolution = 50){

  # can't be from 0 to 1 because when beta and alpha < 0.5,
  # the 0 and 1 becomes Inf -> breaks normalization
  probabilities_v <- seq(0.001, 1 - 0.001, length.out = resolution)
  
  learner_output$marginal_density_b_F_l <- vector(mode = "list",
                                                  game_attributes$n_trial)
  learner_output$marginal_density_g_F_l <- vector(mode = "list",
                                                  game_attributes$n_trial)
  
  for (t in 1:game_attributes$n_trials) {
    learner_output$marginal_density_b_F_l[[t]] <- sum_to_1(dbeta(probabilities_v,
                                                                 learner_output$alpha_b[t], 
                                                                 learner_output$beta_b[t]))
    learner_output$marginal_density_g_F_l[[t]] <- sum_to_1(dbeta(probabilities_v,
                                                                 learner_output$alpha_g[t], 
                                                                 learner_output$beta_g[t]))
  }
  
  return(learner_output)
}




# Function that return the posterior of being in the blue state after having 
# seen the news station first and then the independent council
run_news_station_first_within_belief_update <-  function(
    log_odds_a_I_c_I_independent_council, 
    posteriors_blue_I_vector
  ){

  posterior_blue_final <- 
    log_odds_a_I_c_I_independent_council$posterior_blue * posteriors_blue_I_vector
  posterior_green_final <- 
    (1 - log_odds_a_I_c_I_independent_council$posterior_blue) * (1 - posteriors_blue_I_vector)
  
  denom_for_normalization <- posterior_blue_final + posterior_green_final
  
  posterior_blue_final <- posterior_blue_final / denom_for_normalization

  return(posterior_blue_final)
}
