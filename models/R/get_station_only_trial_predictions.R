# Computes a vector of posteriors for the station-only trials based on
# - a marginal belief state (passed from list_returned_from_run_learner)
# - the game attributes (important for defining the n of draws and trials)
# - likelihood_{b_I,g_I} allows altering the prior over states
# - trial defaults to last trial in block but can be customized
get_station_only_trial_predictions <- function(
    list_returned_from_run_learner,
    game_attributes,
    learner_type,
    likelihood_b_I = .5,
    likelihood_g_I = .5,
    trial_number = FALSE,
    resolution = 50){

  
  learner_output <- list_returned_from_run_learner$learner_output
  
  # Get belief states ----------------------------------------------------------
  
  # The default here is to use the belief state of the final trial
  if (!trial_number) trial_number <- game_attributes$n_trials
  
  # The belief states are saved differently for the two feedback conditions
  if(learner_type$type == "bayes_learner"){
    
    # Get the marginal density and apply forgetting right away
    marginal_density_b_F <- forget_marginal_density(
      learner_output$marginal_density_b_F_l[[trial_number]], learner_type
      )
    
    marginal_density_g_F <-  forget_marginal_density(
      learner_output$marginal_density_g_F_l[[trial_number]], learner_type
      )
    
  } else { # ground truth learner
    
    # We get the parameters - and do the forgetting right away
    alpha_b <- forget_in_with_feedback_condition(
      x = learner_output$alpha_b[trial_number], 
      lambda_forgetting = learner_type$lambda_forgetting,
      forgetting_type = learner_type$forgetting_type,
      forget_towards_x = learner_type$alpha_forget_towards)
    
    beta_b <- forget_in_with_feedback_condition(
      x = learner_output$beta_b[trial_number], 
      lambda_forgetting = learner_type$lambda_forgetting,
      forgetting_type = learner_type$forgetting_type,
      forget_towards_x = learner_type$beta_forget_towards)
    
    alpha_g <- forget_in_with_feedback_condition(
      x = learner_output$alpha_g[trial_number], 
      lambda_forgetting = learner_type$lambda_forgetting,
      forgetting_type = learner_type$forgetting_type,
      forget_towards_x = learner_type$alpha_forget_towards)
    
    beta_g <- forget_in_with_feedback_condition(
      x = learner_output$beta_g[trial_number], 
      lambda_forgetting = learner_type$lambda_forgetting,
      forgetting_type = learner_type$forgetting_type,
      forget_towards_x = learner_type$beta_forget_towards)

    # Here, we need to first set up the marginal densities
    probabilities_v <- seq(0.001, 1 - 0.001, length.out = resolution)
    marginal_density_b_F <- sum_to_1(dbeta(probabilities_v, alpha_b, beta_b))
    marginal_density_g_F <- sum_to_1(dbeta(probabilities_v, alpha_g, beta_g))
  }
  
  
  # Get predictions ------------------------------------------------------------
  
  # Set-up empty vector
  vector_of_posteriors <- 1:game_attributes$n_draws + 1
  
  for (i in 0:(game_attributes$n_draws)) { # note 0-base here
    
    vector_of_posteriors[i+1] <- update_within_trial_belief(
      likelihood_b_I = .5,
      likelihood_g_I = .5,
      X_F = i,
      marginal_density_b_F = marginal_density_b_F,   
      marginal_density_g_F = marginal_density_g_F,
      game_attributes = game_attributes
    )
  }
  
  vector_of_posteriors <- transform_conf(
    vector_of_posteriors, 
    learner_type$conf_transform
    )
  
  return(vector_of_posteriors)
}
