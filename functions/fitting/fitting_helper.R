# Computes the BIC for a model
compute_BIC <- function(
    negative_log_likelihood, # needs to be converted to positive
    number_of_data_points,
    number_of_parameters
){
  
  BIC <- number_of_parameters * log(number_of_data_points) - 2 * (-negative_log_likelihood)
  
  return(BIC)
}

# Function that adds noise to a vector (of confidences)
truncate_and_add_noise <- function(vector, noise_to_be_added){
  
  vector <- rnorm(length(vector), vector, noise_to_be_added)
  vector <- ifelse(vector < 0, 0, vector)
  vector <- ifelse(vector > 1, 1, vector)
}




add_noise_to_df <- function(df, noise_to_be_added){
  # adds noise to the original confidence and saves the original confidence
  # as a different variable
  df <- df %>% 
    mutate(
      c_I_no_noise = c_I,
      c_F_no_noise = c_F,
      c_I = truncate_and_add_noise(c_I, noise_to_be_added),
      c_F = truncate_and_add_noise(c_F, noise_to_be_added),
      posterior_blue_I_no_noise = posterior_blue_I,  
      posterior_blue_F_no_noise = posterior_blue_F,
      posterior_blue_I = truncate_and_add_noise(posterior_blue_I, noise_to_be_added),
      posterior_blue_F = truncate_and_add_noise(posterior_blue_F, noise_to_be_added)
    )
  
  return(df)
}




add_noise_to_station_only <- function(
    list_of_station_only_trials, 
    noise_to_be_added
){
  # adds noise to station only trials (this is for parameter recovery)
  
  sources_to_run <- names(list_of_station_only_trials)
  
  for(current_source in sources_to_run){
    list_of_station_only_trials[[current_source]] <-
      truncate_and_add_noise(
        list_of_station_only_trials[[current_source]], 
        noise_to_be_added
      )
    
  }
  

  return(list_of_station_only_trials)
}




# Function that takes two vectors and returns the pairwise log likelihood
compute_log_likelihood <- function(model_prediction_v,
                                   behaviour_v,
                                   noise_assumed){
  
  log_likelihood_of_data_given_model_predictions <- 
    dnorm(x = behaviour_v,
          mean = model_prediction_v,
          sd = noise_assumed,
          log = TRUE)
  
  return(log_likelihood_of_data_given_model_predictions)
}

# Function that takes vectors of initial and final confidence predictions,
# and vectors of initial and final confidence data, and returns the total
# log-likelihood of that data given the model
get_full_neg_log_likelihood <- function(c_I_model,
                                        c_I_behaviour,
                                        c_F_model,
                                        c_F_behaviour,
                                        station_only_model = FALSE,
                                        station_only_behaviour = FALSE,
                                        confidence_noise){
  # 
  # print("c_I_model and c_I_behaviour")
  # print(c_I_model)
  # print(c_I_behaviour)
  # print("c_F_model and c_F_behaviour")
  # print(c_F_model)
  # print(c_F_behaviour)
  # 
  
  # print(paste("c_I_model:", c_I_model))
  
  # Compute likelihoods per trial
  c_I_log_likelihoods <-
    compute_log_likelihood(c_I_model,
                           c_I_behaviour,
                           confidence_noise)

  c_I_sum_of_neg_log_liks <- - sum(c_I_log_likelihoods)
  print(paste("c_I_logl:", c_I_sum_of_neg_log_liks))
  
  if(is.na(c_F_model[1])){
    negative_log_likelihood <- c_I_sum_of_neg_log_liks
  } else {
    c_F_log_likelihoods <-
      compute_log_likelihood(c_F_model,
                             c_F_behaviour,
                             confidence_noise)
    c_F_sum_of_neg_log_liks <- - sum(c_F_log_likelihoods)
    negative_log_likelihood <- c_I_sum_of_neg_log_liks + c_F_sum_of_neg_log_liks
    print(paste("c_F_logl:", c_F_sum_of_neg_log_liks))
  }
  
  # print(paste("station_only_behaviour:", station_only_behaviour))
  
  if(station_only_behaviour[[1]] != FALSE){
    # 
    # print("station_only_model then behaviour")
    # print(station_only_model)
    # print(station_only_behaviour)
    
    # Compute likelihood per trial
    station_only_log_likelihoods <- 
      compute_log_likelihood(station_only_model,
                             station_only_behaviour,
                             confidence_noise)
    
    # Sum these (notice the + in the second line because all arguments are
    # already )
    station_only_sum_of_neg_log_lik <-  - sum(station_only_log_likelihoods)
    negative_log_likelihood <- negative_log_likelihood + station_only_sum_of_neg_log_lik
    
    print(paste("station_only_logl:", station_only_sum_of_neg_log_lik))
  
  }
  
  return(negative_log_likelihood)
}


# Function that takes a full game dataframe and only picks one source out of it
subset_game_df_by_source <- function(df_with_many_sources, source_to_pick){
  df_out <- df_with_many_sources %>% 
    filter(source_type == source_to_pick)
  return(df_out)
}


# Sample parameters for parameter recovery
sample_parameters_for_recovery <- function(n_to_recover,
                                           prior_range,
                                           lambda_sample_parameters,
                                           weight_range,
                                           forget_towards_range,
                                           noise_range,
                                           conf_transform_range,
                                           feedback_conditions,
                                           game_attributes,
                                           b_g_prior_the_same = T){
  
  # Main set-up of dataframe
  df_parameters_for_recovery <- 
    data.frame(type = feedback_conditions,
               alpha_b_prior = runif(n_to_recover, prior_range[1], prior_range[2]),
               beta_b_prior = runif(n_to_recover, prior_range[1], prior_range[2]),
               alpha_g_prior = NA,
               beta_g_prior = NA,
               lambda_forgetting = rbeta(n_to_recover, 
                                         lambda_sample_parameters[1],
                                         lambda_sample_parameters[2]),
               alpha_weight = runif(n_to_recover, 
                                    weight_range[1], 
                                    weight_range[2]),
               beta_weight = runif(n_to_recover, 
                                   weight_range[1], 
                                   weight_range[2]),
               alpha_forget_towards = runif(n_to_recover, 
                                            forget_towards_range[1],
                                            forget_towards_range[2]),
               beta_forget_towards = runif(n_to_recover, 
                                           forget_towards_range[1], 
                                           forget_towards_range[2]),
               confidence_noise = runif(n_to_recover, 
                                        noise_range[1],
                                        noise_range[2]))
  
  # use a log-scale for the conf_transform
  df_parameters_for_recovery$conf_transform <- 
    exp(
    runif(n_to_recover, 
          log(conf_transform_range[1]),
          log(conf_transform_range[2]))
    )
  
  # Either use same prior for b_F and g_F or not
  if (b_g_prior_the_same) {
    df_parameters_for_recovery <- df_parameters_for_recovery %>% 
      mutate(alpha_g_prior = alpha_b_prior,
             beta_g_prior = beta_b_prior)
  } else {
    df_parameters_for_recovery$alpha_g_prior <-
      runif(n_to_recover, prior_range[1], prior_range[2])
    df_parameters_for_recovery$beta_g_prior <- 
      runif(n_to_recover, prior_range[1], prior_range[2])
  }

  # Compute the beta mean and uncertainty
  df_parameters_for_recovery <- df_parameters_for_recovery %>% 
    mutate(beta_b_mean = alpha_b_prior / (alpha_b_prior + beta_b_prior),
           beta_b_uncertainty = alpha_b_prior + beta_b_prior,
           beta_g_mean = alpha_g_prior / (alpha_g_prior + beta_g_prior),
           beta_g_uncertainty = alpha_g_prior + beta_g_prior)
  
  # Set the subjective b_I and g_I parameters
  df_parameters_for_recovery$b_I_subjective <- game_attributes$b_I
  df_parameters_for_recovery$g_I_subjective <- game_attributes$g_I
  df_parameters_for_recovery$variational <- F
  
  return(df_parameters_for_recovery)
}


################################################################################
################################################################################

get_vector_of_parameters_to_fit <- function(
    feedback_condition,
    how_to_fit_beta,
    parameters_not_to_fit,
    limited_model,
    conf_transform){
  
  # Assign beta mean and uncertainty or alpha/beta
  if (how_to_fit_beta == "mean_uncertainty") {
    
    # Choose prior
    if(limited_model == FALSE){
      parameters_to_fit <- 
        c("beta_b_mean", "beta_b_uncertainty", "beta_g_mean", "beta_g_uncertainty")
    } else{
      
      if(limited_model %in% c("one_prior", "one_prior_and_forgetting")) {
        parameters_to_fit <- c("prior_mean", "prior_uncertainty")
      }
    }
    
    
  } else if (how_to_fit_beta == "individual"){
    
    if(limited_model != FALSE){
      warning("Fitting individual betas - no limited models specificed")
    }
    
    parameters_to_fit <- c("alpha_b_prior", "beta_b_prior",
                           "alpha_g_prior", "beta_g_prior")
    
  }
  
  # Parameters depending on source type
  if (feedback_condition == "ground_truth") {
    
    if(limited_model %in% c(FALSE, "one_prior")){
      parameters_to_fit <- c(parameters_to_fit,
                             "lambda_forgetting",
                             "alpha_weight",
                             "beta_weight",
                             "confidence_noise")
    } else if(limited_model == "one_prior_and_forgetting"){
      parameters_to_fit <- c(parameters_to_fit,
                             "lambda_forgetting",
                             "confidence_noise")
    }
    
  } else if (feedback_condition == "bayes_learner"){
    
    if(limited_model %in% c(FALSE, "one_prior")){
      parameters_to_fit <- c(parameters_to_fit,
                             "lambda_forgetting",
                             "alpha_forget_towards",
                             "beta_forget_towards",
                             "confidence_noise")
    } else if(limited_model == "one_prior_and_forgetting"){
      parameters_to_fit <- c(parameters_to_fit,
                             "lambda_forgetting",
                             "confidence_noise")
    }
  
  }
  
  if(conf_transform){
    parameters_to_fit <- c(parameters_to_fit, "conf_transform")
  }

  # Throw out parameters
  if (length(parameters_not_to_fit) > 0) {
    index_to_remove_parameters <- parameters_to_fit %in% parameters_not_to_fit
    parameters_to_fit <- parameters_to_fit[! index_to_remove_parameters]
  }
  
  return(parameters_to_fit)
}


# Gets a list of standard parameter ranges to fit
get_standard_parameter_ranges <- function(
    prior_range = c(.8, 40),
    lambda_sample_parameters = c(1, 4),
    weight_range = c(.5, 2),
    forget_towards_range = c(.8, 40),
    noise_range = c(0.005, 0.35),
    conf_transform_range = c(0.25, 10)){
  
  parameter_ranges <- list(
    prior_range = prior_range,
    lambda_sample_parameters = lambda_sample_parameters,
    weight_range = weight_range,
    forget_towards_range = forget_towards_range,
    noise_range = noise_range,
    conf_transform_range = conf_transform_range 
  )
  
  return(parameter_ranges)
}


prep_parameters_for_fitting <- function(
    feedback_condition,
    prior_range = NULL,
    weight_range = NULL,
    noise_range = NULL,
    forget_towards_range = NULL,
    how_to_fit_beta = "mean_uncertainty",
    parameters_not_to_fit = NA,
    limited_model = FALSE,
    conf_transform_range = FALSE){
  # Function that preps the parameters for fitting
  # Returns: 
  # - Bounds for DEOPTIM in the correct format
  # - List of parameters to fit
  
  parameters_to_fit <- get_vector_of_parameters_to_fit(
    feedback_condition = feedback_condition,
    how_to_fit_beta = how_to_fit_beta,
    parameters_not_to_fit = parameters_not_to_fit,
    limited_model = limited_model,
    conf_transform = length(conf_transform_range) > 1 # gets this if !FALSE
  )
  
  bounds <- list()
  
  # Assign beta mean and uncertainty or alpha/beta
  if (how_to_fit_beta == "mean_uncertainty") {
    
    # Choose prior
    if(limited_model == FALSE){
      number_of_priors <- 2
    } else if(limited_model %in% c("one_prior", "one_prior_and_forgetting")) {
      number_of_priors <- 1
    }
  
    # The beta means and uncertainties
    bounds$lower <- rep(c(0.01,prior_range[1] * 2),
                        number_of_priors)
    bounds$upper <- rep(c(0.99,prior_range[2] * 2),
                        number_of_priors)
    
    # Lambda forgetting
    bounds$lower <- c(bounds$lower, 0)
    bounds$upper <- c(bounds$upper, 1)
    
  } else if (how_to_fit_beta == "individual"){
    
    warning("Fitting individual betas - no limited models specificed")
    
    # The beta means and uncertainties
    bounds$lower <- rep(prior_range[1],4)
    bounds$upper <- rep(prior_range[2],4)
    
    # Lambda forgetting
    bounds$lower <- c(bounds$lower, 0)
    bounds$upper <- c(bounds$upper, 1)
    
  }
  
  # Parameters depending on source type
  if(limited_model != "one_prior_and_forgetting"){
    
    if (feedback_condition == "ground_truth") {
      
      # alpha and beta weights
      bounds$lower <- c(bounds$lower,rep(weight_range[1], 2))
      
      bounds$upper <- c(bounds$upper,rep(weight_range[2], 2))
      
    } else if (feedback_condition == "bayes_learner"){
      
      # alpha and beta weights
      bounds$lower <- c(bounds$lower,rep(forget_towards_range[1], 2))
      
      bounds$upper <- c(bounds$upper,rep(forget_towards_range[2], 2))
      
    }
    
  }
  
  # confidence noise
  bounds$lower <- c(bounds$lower, noise_range[1])
  bounds$upper <- c(bounds$upper, noise_range[2]) 
  
  if(length(conf_transform_range) > 1){
    # non-linear scaling of confidence
    bounds$lower <- c(bounds$lower, conf_transform_range[1])
    bounds$upper <- c(bounds$upper, conf_transform_range[2])
  }

  
  # Throw out parameters
  if (length(parameters_not_to_fit) > 0) {
    index_to_remove_parameters <- parameters_to_fit %in% parameters_not_to_fit
    bounds$lower <- bounds$lower[! index_to_remove_parameters]
    bounds$upper <- bounds$upper[! index_to_remove_parameters]
  }

  # Saving all to one list
  parameter_attributes <- list(parameters_to_fit = parameters_to_fit,
                               bounds = bounds)
  
  return(parameter_attributes)
}


################################################################################

print_parameters_to_fit_and_their_bounds <- function(parameter_attributes){
  # Function that produces a pretty overview over the parameters we fit and 
  # their bounds
  bounds_to_print <- data.frame(parameter = parameter_attributes$parameters_to_fit,
                                lower_bound = parameter_attributes$bounds$lower,
                                upper_bound = parameter_attributes$bounds$upper)
  
  print.data.frame(bounds_to_print)
}



################################################################################


# Gets a list of functions used for the fitting - this is necessary for multi-
# core fitting where these variables need to be passed to a cluster 
get_vector_of_functions_used_for_fitting <- function(){
  
  vector_of_functions_used <- c()
  
  for (agent_function_file in agent_function_files) {
    list_of_functions_used <- 
      list.functions.in.file(here(agent_function_file), alphabetic = TRUE)
    vector_of_functions_used <- c(vector_of_functions_used,
                                  list_of_functions_used$.GlobalEnv)
  }
  
  for(fitting_function in fitting_function_files){
    list_of_functions_used <- 
      list.functions.in.file(here(fitting_function), alphabetic = TRUE)
    vector_of_functions_used <- c(vector_of_functions_used,
                                  list_of_functions_used$.GlobalEnv)
  }
  
  vector_of_functions_used <- unique(vector_of_functions_used)
  
  return(vector_of_functions_used) 
}



