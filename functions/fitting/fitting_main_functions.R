
get_neg_log_lik_with_flexible_parameters <- function(
  # Function that computes the negative log-likelihood based on data and 
  # settings for one source
    x,
    df_data, # df with ONE source
    parameters_to_fit,
    game_attributes, 
    fixed_learner_attributes_df,
    station_only_trials = FALSE){
  
  # Unify the names of the columns (empirical and model)
  # Might want to unify this more outside of this function later
  if("num_blue_indep" %in% colnames(df_data)){
    df_data$X_I <- df_data$num_blue_indep
    df_data$X_F <- df_data$num_blue_news
  }
  
  # Replaced fixed values with currently entered values (through x)
  fixed_learner_attributes_df[, parameters_to_fit] <- x
  
  # print(fixed_learner_attributes_df)
  
  # Get the model predictions
  run_station_only_trials <- ifelse(station_only_trials[1] != FALSE, TRUE, FALSE)
  
  run_learner_output <- 
    run_learner_on_game_and_station_only(
      df_game = df_data,
      learner_type = fixed_learner_attributes_df,
      game_attributes = game_attributes,
      station_only = run_station_only_trials
  )

  # Get the log-likelihood
  neg_log_likelihood <- 
    get_full_neg_log_likelihood(
      c_I_model = run_learner_output$df_game$posterior_blue_I,
      c_I_behaviour = df_data$posterior_blue_I,
      c_F_model = run_learner_output$df_game$posterior_blue_F,
      c_F_behaviour = df_data$posterior_blue_F,
      station_only_model = run_learner_output$station_only_trials,
      station_only_behaviour = station_only_trials,
      confidence_noise = fixed_learner_attributes_df$confidence_noise
    )
  
  return(neg_log_likelihood)
}


get_neg_log_lik_several_sources <- function(
    x, # parameters that are entered through the optimization function (can have variable length)
    df_full_game, # dataframe with game info and data (SEVERAL sources)
    parameters_to_fit, # vector of parameters that will be fit
    game_attributes, # game_attributes define parameters of the game
    fixed_learner_attributes_df, # Contains params that will not be fit
    sources_to_run = c("helpful","random","opposite","greenbias","bluebias"),
    station_only_trials = FALSE, # whether to run the station only trials or not
    limited_model = FALSE,
    conf_transform = FALSE,
    learn_one_likelihood = FALSE
    ){
  # Function that gets the log-likelihood for a dataset with several sources
  
  if(length(x) != length(parameters_to_fit)){
    warning("Parameter values vector x doesn't have same length as 'parameters to fit")
    return()
  }

  # In case we use a limited model, get the appropriate set-up
  processed_parameter_values_and_names <- define_parameters_for_limited_models(
    x = x,
    parameters_to_fit = parameters_to_fit,
    limited_model = limited_model,
    fixed_learner_attributes_df = fixed_learner_attributes_df,
    conf_transform = conf_transform
  )
  
  print(processed_parameter_values_and_names)
  
  print("define parameters done")  
  
  x <- processed_parameter_values_and_names$x
  parameters_to_fit <- processed_parameter_values_and_names$parameters_to_fit
  fixed_learner_attributes_df <-
    processed_parameter_values_and_names$fixed_learner_attributes_df
  
  # Set up empty vector to be filled with negative log-likelihoods
  neg_log_lik_v <- 1:length(sources_to_run)
  
  print("get_neg_log_lik_several_sources set-up successful")
  
  # Get the neg log lik for each source individually
  for (i in 1:length(sources_to_run)) {
    
    # Subset the df for the two dataframes as well as the news station only 
    # trials (if necessary)
    df_only_one_source <- subset_game_df_by_source(df_full_game, sources_to_run[i])
    station_only_trials_one_source <- ifelse(
      station_only_trials[[1]] == FALSE, # needs to be 'pseudo'-subset when FALSE 
      FALSE, # so that station only trials are not fit
      station_only_trials[[sources_to_run[i]]]
    )
    
    neg_log_lik_v[i] <- 
      get_neg_log_lik_with_flexible_parameters(
        x = x,
        df_data = df_only_one_source,
        parameters_to_fit = parameters_to_fit,
        game_attributes = game_attributes, 
        fixed_learner_attributes_df = fixed_learner_attributes_df,
        station_only_trials = station_only_trials_one_source
      )
  }
  
  print(neg_log_lik_v)
  
  # ... then sum and return it
  return(sum(neg_log_lik_v))
}


define_parameters_for_limited_models <- function(
    x,
    parameters_to_fit,
    limited_model = FALSE,
    conf_transform = FALSE,
    learn_one_likelihood = FALSE,
    fixed_learner_attributes_df
  ){
  # Returns a list of
  # - x (vector of parameter values passed down in fitting)
  # - parameters_to_fit (vector of parameter names)
  # - fixed_learner_attributes_df
  
  # print("Define parameters")
  # print(conf_transform)
  # print(limited_model)
  
  if(limited_model != FALSE){
    

    if(limited_model == "one_prior"){
      x <- c(x[1], x[2], x) # duplicates prior - because full model is 'fit'
      
    } else if (limited_model == "one_prior_and_forgetting"){
      
      # thus far forgetting parameters are entered as alpha/beta - so we need 
      # to transform them
      params_for_forgetting <- get_beta_parameters_from_summary(x[1],x[2])
      
      # prior mean, uncertainty (*2), forgetting lambda, prior m, unc, noise
      new_x <- c(
        x[1], x[2], # priors 
        x[1], x[2], # priors 
        x[3], # forgetting
        params_for_forgetting$a, params_for_forgetting$b, 
        x[4]
      ) 
      
      
      if(conf_transform){
        x <- c(new_x, x[5])
      } else {
        x <- new_x
      }
      
      # Make sure forgetting is set to the correct type
      fixed_learner_attributes_df$forgetting_type <- "forget_towards"
    }
    
    # print("Readjusting the parameters to fit")
    
    # because we just replicate the parameters above, we still need to supply
    # the full list of parameters to get_neg_log_lik_with_flexible_parameters
    parameters_to_fit <- get_vector_of_parameters_to_fit(
      feedback_condition = fixed_learner_attributes_df$type,
      how_to_fit_beta = "mean_uncertainty",
      parameters_not_to_fit = c(),
      limited_model = FALSE,
      conf_transform = conf_transform
    )
  }
  
  fixed_learner_attributes_df$learn_one_likelihood <- learn_one_likelihood
  
  return(
    list(
      x = x, 
      parameters_to_fit = parameters_to_fit,
      fixed_learner_attributes_df = fixed_learner_attributes_df)
  )
}
