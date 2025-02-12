# Functions that run the learning of the trustworthiness of a source
#

run_ground_truth_learner <- 
  function(ground_truth, # vector of either ground_truth or a_i
           X_F,          # vector of number of blue votes
           learner_type = NULL,
           n_draws){
    # Function that runs the learner WITH feedback on a vector of X_Fs and states
    # specifying the resolution and learner_type
    
    if(learner_type$forgetting_type == "forget_towards"){
      # In the forget towards case, we (for now) do not weigh the alpha and 
      # beta update. Instead, we mix in a distribution whose parameters
      # we define here. Note that in the current (14.03.22) version - this
      # is passed down via the prior (limited_model == "one_prior_and_forgetting")
      learner_type$alpha_forget_towards <- learner_type$alpha_weight
      learner_type$beta_forget_towards <- learner_type$beta_weight
      learner_type$alpha_weight <- 1
      learner_type$alpha_weight <- 1
    }
    
    # Set up vectors to fill with alpha and beta parameters
    n_trials <- length(ground_truth)
    
    beta_b_parameter_list <-
      get_beta_parameters_from_summary(learner_type$beta_b_mean,
                                       learner_type$beta_b_uncertainty)
    beta_g_parameter_list <- 
      get_beta_parameters_from_summary(learner_type$beta_g_mean,
                                       learner_type$beta_g_uncertainty)
    
    learner_type$alpha_b_prior <- beta_b_parameter_list$a
    learner_type$beta_b_prior <- beta_b_parameter_list$b
    learner_type$alpha_g_prior <- beta_g_parameter_list$a
    learner_type$beta_g_prior <- beta_g_parameter_list$b
    
    alpha_g <- rep(learner_type$alpha_g_prior,
                       n_trials)
    beta_g <- rep(learner_type$beta_g_prior,
                      n_trials)
    alpha_b <- rep(learner_type$alpha_b_prior,
                      n_trials)
    beta_b <- rep(learner_type$beta_b_prior,
                     n_trials)
    
    # Loop over trials and update the beta distributions
    for (t in 1 : (n_trials - 1)) {
      
      # State-dependent update
      if (is.null(learner_type$only_learn_one_likelihood)){
        
        if (ground_truth[t] == -1) {
          
          # if ground truth is green: update green parameters
          alpha_g[t+1] <- alpha_g[t] + learner_type$alpha_weight * (n_draws - X_F[t])
          beta_g[t+1]<- beta_g[t] + learner_type$beta_weight * X_F[t]
          
          # ... and leave blue parameters untouched
          alpha_b[t+1] <- alpha_b[t] 
          beta_b[t+1] <- beta_b[t] 
          
        } else if (ground_truth[t] == 1){
          
          # if ground truth is blue: leave green parameters untouched
          alpha_g[t+1] <- alpha_g[t]
          beta_g[t+1]<- beta_g[t] 
          
          # ... and update the blue parameters
          alpha_b[t+1] <- alpha_b[t] + learner_type$alpha_weight * X_F[t]
          beta_b[t+1] <- beta_b[t] +  learner_type$beta_weight * (n_draws - X_F[t])
        }
        
      } else {
        # We do the updating with one likelihood
        # For convenience, we keep the two likelihoods but force them to be
        # equivalent throughout
        
        if (ground_truth[t] == -1) {
          
          alpha_g[t+1] <- alpha_g[t] + learner_type$alpha_weight * (n_draws - X_F[t])
          beta_g[t+1]<- beta_g[t] + learner_type$beta_weight * X_F[t]
          alpha_b[t+1] <- alpha_g[t+1]
          beta_b[t+1] <- beta_g[t+1]
          
        } else if (ground_truth[t] == 1){
          
          alpha_b[t+1] <- alpha_b[t] + learner_type$alpha_weight * X_F[t]
          beta_b[t+1] <- beta_b[t] +  learner_type$beta_weight * (n_draws - X_F[t])
          alpha_g[t+1] <- alpha_b[t+1]
          beta_g[t+1]<- beta_b[t+1]
        }
        
      }

      
      # Forgetting - note that this happens after the update at the moment
      # But we could equally imagine that it might happen before
      if (learner_type$lambda_forgetting != 0) {
        
        alpha_b[t + 1] <- forget_in_with_feedback_condition(
          x = alpha_b[t + 1], 
          lambda_forgetting = learner_type$lambda_forgetting,
          forgetting_type = learner_type$forgetting_type,
          forget_towards_x = learner_type$alpha_forget_towards)
        
        beta_b[t + 1] <- forget_in_with_feedback_condition(
          x = beta_b[t + 1], 
          lambda_forgetting = learner_type$lambda_forgetting,
          forgetting_type = learner_type$forgetting_type,
          forget_towards_x = learner_type$beta_forget_towards)
        
        alpha_g[t + 1] <- forget_in_with_feedback_condition(
          x = alpha_g[t + 1], 
          lambda_forgetting = learner_type$lambda_forgetting,
          forgetting_type = learner_type$forgetting_type,
          forget_towards_x = learner_type$alpha_forget_towards)
        
        beta_g[t + 1] <- forget_in_with_feedback_condition(
          x = beta_g[t + 1], 
          lambda_forgetting = learner_type$lambda_forgetting,
          forgetting_type = learner_type$forgetting_type,
          forget_towards_x = learner_type$beta_forget_towards)

      }
    }
    
    # First, get the alphas and betas
    output_dataframe <- data.frame(alpha_g, beta_g, alpha_b, beta_b)

    # Then compute the respective estimates
    output_dataframe <- output_dataframe %>% 
      mutate(mean_estimate_b_F = alpha_b /(alpha_b + beta_b)) %>%
      mutate(mean_estimate_g_F = alpha_g /(alpha_g + beta_g))  
      
    return(output_dataframe)
}




# Function used for forgetting in the learner WITH feedback
# Normal forgetting
forget_in_with_feedback_condition <- function(
    x, # the parameters (alpha_b, etc.),
    lambda_forgetting,
    forgetting_type,
    forget_towards_x = NULL # only used in case of forget towards
){
  
  if(forgetting_type == "forget_towards"){
    
    x <- forget_towards_alpha_beta(x, lambda_forgetting, forget_towards_x)
    
  } else if(forgetting_type == "noise"){
    
    x <- forget_alpha_beta(x, lambda_forgetting)
  }
  
  return(x)
  
}

# Simple noise forgetting
forget_alpha_beta <- function(x, lambda_forgetting){
  x <- x * (1 - lambda_forgetting) + lambda_forgetting
  return(x)
}

# Forgetting towards
forget_towards_alpha_beta <- function(x, lambda_forgetting, forget_towards_x){
  x <- x * (1 - lambda_forgetting) + lambda_forgetting * forget_towards_x
  return(x)
}


################################################################################


run_bayesian_learner <- function(
    X_I, 
    X_F,
    game_attributes,
    resolution,
    learner_type){
  # Function that runs the WITHOUT feedback learner on a vector of X_I and X_F
  # given some game_attributes, and specifying the resolution and learner_type
  
  # Set up data for belief matrices and marginal distributions
  bayes_learner_belief_l <- vector(mode = "list", game_attributes$n_trial)
  marginal_density_b_F_l <- vector(mode = "list", game_attributes$n_trial)
  marginal_density_g_F_l <- vector(mode = "list", game_attributes$n_trial)
  
  mean_estimate_b_F <- 1:game_attributes$n_trials
  mean_estimate_g_F <- 1:game_attributes$n_trials
  
  
  # Set up prior matrix
  
  beta_b_parameter_list <-
    get_beta_parameters_from_summary(learner_type$beta_b_mean,
                                     learner_type$beta_b_uncertainty)
  beta_g_parameter_list <- 
    get_beta_parameters_from_summary(learner_type$beta_g_mean,
                                     learner_type$beta_g_uncertainty)
  
  learner_type$alpha_b_prior <- beta_b_parameter_list$a
  learner_type$beta_b_prior <- beta_b_parameter_list$b
  learner_type$alpha_g_prior <- beta_g_parameter_list$a
  learner_type$beta_g_prior <- beta_g_parameter_list$b
  
  probability_vector <- seq(0.001, 1 - 0.001, length.out = resolution)
  
  bayes_learner_belief_l[[1]] <- get_belief_state_matrix(
    learner_type$alpha_b_prior, 
    learner_type$beta_b_prior,
    learner_type$alpha_g_prior, 
    learner_type$beta_g_prior,
    probability_vector)
  
  # Run the learner - this for loop only gets the belief states
  for (t in 1:(game_attributes$n_trials-1)) {
    
    if(learner_type$lambda_forgetting != 0){
      matrix_to_forget_towards <- get_belief_state_matrix(
        learner_type$alpha_forget_towards, 
        learner_type$beta_forget_towards,
        learner_type$alpha_forget_towards, 
        learner_type$beta_forget_towards,
        probability_vector)
    }
    
    # do actual learning
    bayes_learner_belief_l[[t + 1]] <- update_source_belief(
        prior_matrix = bayes_learner_belief_l[[t]],
        matrix_to_forget_towards = matrix_to_forget_towards,
        X_I = X_I[t],
        X_F = X_F[t],
        game_attributes,
        resolution,
        learner_type)
  }
  
  # Get summary statistics
  for (t in 1:game_attributes$n_trials) {
    # save mean estimate
    marginal_density_b_F_l[[t]] <- rowSums(bayes_learner_belief_l[[t]])
    marginal_density_g_F_l[[t]] <- colSums(bayes_learner_belief_l[[t]])
    
    mean_estimate_b_F[t] <- sum(probability_vector * marginal_density_b_F_l[[t]])
    mean_estimate_g_F[t] <- sum(probability_vector * marginal_density_g_F_l[[t]])
  }
  
  output <- list()
  output$belief_matrix <- bayes_learner_belief_l
  output$mean_estimate_b_F <- mean_estimate_b_F
  output$mean_estimate_g_F <- mean_estimate_g_F
  output$marginal_density_b_F_l <- marginal_density_b_F_l
  output$marginal_density_g_F_l <- marginal_density_g_F_l
  
  return(output)
}


get_belief_state_matrix <- function(
    alpha_b_prior,
    beta_b_prior,
    alpha_g_prior,
    beta_g_prior,
    probability_vector){
  # Function that gets the initial distribution for the prior (in the WITHOUT feedback case)
  
  density_b_F <- sum_to_1(dbeta(probability_vector, 
                                alpha_b_prior, 
                                beta_b_prior))
  density_g_F <- sum_to_1(dbeta(probability_vector, 
                                alpha_g_prior, 
                                beta_g_prior))
  
  return(outer(density_b_F, density_g_F))
}

# Forget towards on a marginal density -> this is used in the station only trials
forget_marginal_density <- function(
    belief, 
    learner_type){
  
  probabilities_v <- seq(0.001, 1 - 0.001, length.out = length(belief))
  forget_towards_belief <- sum_to_1(dbeta(probabilities_v,
                                          learner_type$alpha_forget_towards, 
                                          learner_type$beta_forget_towards))
  
  
  belief <- 
    (1 - learner_type$lambda_forgetting) * belief +
    learner_type$lambda_forgetting * forget_towards_belief
  
  return(belief)
}
