# This produces a dataframe with fit model predictions and data
get_df_predictions_from_model <- function(
    list_of_fits, # one fit list
    df_main_task_wide, # the actual data
    df_ML_fit_parameters_one_fit, # a dataframe with the fit parameters
    limited_model_to_simulate,
    conf_transform_to_simulate
){
  
  # First, we get everything into a list ---------------------------------------
  
  list_of_model_predictions <- list()
  
  participants_to_fit <- names(list_of_fits)
  
  for(current_participant in participants_to_fit){
    
    # Get game and delete original responses
    df_not_played_with_data <- data_for_fitting[[current_participant]]$df_main_trials_all_sources
    df_not_played <- df_not_played_with_data %>%
      select(-posterior_blue_I, posterior_blue_F, -change_of_mind)
    
    # Get game attributes
    current_sources_to_run <- as.vector(unique(df_not_played$source_type))
    current_game_attributes <- data_for_fitting[[current_participant]]$game_attributes
    
    # Get learner attributes
    learner_type_with_fit_parameters <- df_ML_fit_parameters_one_fit %>% 
      filter(completion_code == current_participant)
    
    learner_type_with_fit_parameters <- process_learner_type_from_limited_models(
      learner_type = learner_type_with_fit_parameters,
      limited_model_to_simulate = limited_model_to_simulate,
      conf_transform_to_simulate = conf_transform_to_simulate,
      data_for_fitting_current_participant = data_for_fitting[[current_participant]]
    )
    
    
    list_of_model_predictions[[current_participant]] <- 
      run_learner_on_game_with_several_sources(
        df_input_not_played = df_not_played,
        sources_to_run = current_sources_to_run,
        learner_type = learner_type_with_fit_parameters,
        game_attributes = current_game_attributes,
        agent_name = current_participant,
        run_station_only_trials = TRUE
      )
  }
  
  
  # We then add this all into one dataframe ------------------------------------
  
  df_model_predictions <- 
    list_of_model_predictions[[participants_to_fit[1]]]$df_played_game
  
  for (i in 2:length(list_of_model_predictions)) {
    df_model_predictions <- rbind(
      df_model_predictions,
      list_of_model_predictions[[i]]$df_played_game
    )
  }

  
  
  # And re-add the actual responses --------------------------------------------
  df_main_task_wide_only_main <- df_main_task_wide %>% 
    filter(block_stage == "normal")
  
  if(all(df_main_task_wide_only_main$X_I == df_model_predictions$X_I) & 
     all(df_main_task_wide_only_main$X_F == df_model_predictions$X_F)){
    
    df_model_predictions$posterior_blue_I_data <- df_main_task_wide_only_main$posterior_blue_I
    df_model_predictions$posterior_blue_F_data <- df_main_task_wide_only_main$posterior_blue_F
    
  } else{
    
    warning("dataframes don't line up")
    
  }
  
  # Add changes of mind
  df_model_predictions %<>% 
    mutate(
      change_of_mind = posterior_blue_F - posterior_blue_I,
      change_of_mind_data = posterior_blue_F_data - posterior_blue_I_data
    )
  
  
  # Station only trials --------------------------------------------------------
  
  # We set up an empty df
  df_station_only_predictions <- data.frame(
    completion_code = "a",
    source_type = "a",
    X_F = 1,
    posterior_blue_F = 1,
    posterior_blue_F_data = 1,
    condition = "a",
    news_station_first = "a"
    
  )[NULL,]
  
  for (current_participant in names(list_of_model_predictions)) {
    
    current_station_only <- list_of_model_predictions[[current_participant]]$station_only_trials
    current_station_only_data <- data_for_fitting[[current_participant]]$station_only_trials
    current_sources_to_run <- names(current_station_only)
    
    for(current_source_name in current_sources_to_run){
      df_to_add <- data.frame(
        completion_code = current_participant,
        source_type = current_source_name,
        X_F = 0:5,
        posterior_blue_F = current_station_only[[current_source_name]],
        posterior_blue_F_data = current_station_only_data[[current_source_name]],
        condition = 
          list_of_model_predictions[[current_participant]]$df_played_game$condition[1],
        news_station_first = 
          list_of_model_predictions[[current_participant]]$df_played_game$news_station_first[1]
      )
      
      df_station_only_predictions <- rbind(
        df_station_only_predictions,
        df_to_add
      )
    }
  }
  
  
  
  # Adding the information about the model used (using as character for cross compatibility
  # between FALSE and string descriptions of model)
  df_model_predictions$with_conf_transform <- as.character(conf_transform_to_simulate)
  df_model_predictions$limited_model <- as.character(limited_model_to_simulate)
  df_station_only_predictions$with_conf_transform <- as.character(conf_transform_to_simulate)
  df_station_only_predictions$limited_model <- as.character(limited_model_to_simulate)
  
  list_to_return <- list(
    df_model_predictions = df_model_predictions,
    df_station_only_predictions = df_station_only_predictions
  )
  
  return(list_to_return)
  
}



# Helper function for the above: 
# Takes a learner type, and unifies it to work with run_leaner_on_game_with_several_sources
# This is important for the limited models where different parameters are fit
process_learner_type_from_limited_models <- function(
    learner_type,
    limited_model_to_simulate,
    conf_transform_to_simulate,
    data_for_fitting_current_participant
){
  
  # Learner type attributes that are applicable to all models
  learner_type %<>% 
    mutate(
      type = data_for_fitting_current_participant$learner_type$type,
      forgetting_type = 
        ifelse(
          limited_model_to_simulate == "one_prior_and_forgetting",
          "forget_towards", 
          "noise"
        ),
      b_I_subjective = data_for_fitting_current_participant$game_attributes$b_I,
      g_I_subjective = data_for_fitting_current_participant$game_attributes$g_I,
      variational = FALSE
    )
  
  
  # Model specific aspects
  if (limited_model_to_simulate %in% c("one_prior",  "one_prior_and_forgetting")) {
    
    # save the prior
    learner_type %<>%
      mutate(
        beta_b_mean = prior_mean,
        beta_g_mean = prior_mean,
        beta_b_uncertainty = prior_uncertainty,
        beta_g_uncertainty = prior_uncertainty
      )
    
    # change the forgetting
    if(limited_model_to_simulate == "one_prior_and_forgetting"){
      
      beta_parameters_for_forgetting <- 
        get_beta_parameters_from_summary(
          learner_type$prior_mean,
          learner_type$prior_uncertainty
        )
      
      learner_type %<>% 
        mutate(
          alpha_weight = beta_parameters_for_forgetting$a,
          beta_weight = beta_parameters_for_forgetting$b,
          alpha_forget_towards = beta_parameters_for_forgetting$a,
          beta_forget_towards = beta_parameters_for_forgetting$b
        )
      
    }
  }
  
  if(conf_transform_to_simulate == FALSE){
    learner_type$conf_transform <- 1
  }
  
  return(learner_type)
}



# Function that pivots the df_model_predictions - this is used to plot posterior
# predictive checks
pivot_df_model_predictions_to_longer <- function(
    df_model_predictions,
    news_source_only_conditon = FALSE
    ){
  
  
  # We pivot this for the individual outcomes
  df_model_predictions_long1 <- df_model_predictions %>% 
    pivot_longer(
      cols = c("posterior_blue_I", "posterior_blue_I_data"),
      names_to = "model_or_data", 
      values_to = "posterior_blue_I"
      ) 
  
  # ... and then combine the two
  df_model_predictions_long <- df_model_predictions_long1
  
  if (!news_source_only_conditon) {
    df_model_predictions_long2 <- df_model_predictions %>% 
      pivot_longer(
        cols = c("posterior_blue_F", "posterior_blue_F_data"), 
        names_to = "model_or_data", 
        values_to = "posterior_blue_F"
      )
    
    df_model_predictions_long3 <- df_model_predictions %>% 
      pivot_longer(
        cols = c("change_of_mind", "change_of_mind_data"), 
        names_to = "model_or_data", 
        values_to = "change_of_mind"
      )
    
    df_model_predictions_long$posterior_blue_F <- df_model_predictions_long2$posterior_blue_F
    df_model_predictions_long$change_of_mind <- df_model_predictions_long3$change_of_mind

    
    df_model_predictions_long %<>%
      mutate(model_or_data = ifelse(model_or_data == "posterior_blue_I", "model", "data")) %>% 
      select(-posterior_blue_F_data, -change_of_mind_data)
        
  }

  
  return(df_model_predictions_long)
  
}
