# Function that simulates games for parameter fitting
# Returns
#  - list of list (models) of list (transform) of participants
simulate_games_for_parameter_recovery_with_different_options <- function(
    vector_of_limited_models,
    vector_of_conf_transform_options,
    df_parameters_for_recovery, # dataframe of true parameters for generation
    list_of_raw_game_dfs,
    list_of_game_attributes,
    sources_to_run = c("helpful", "random", "opposite", "bluebias")
){
  
  n_to_recover <- length(list_of_raw_game_dfs)
  run_station_only_trials <- TRUE
  list_of_simulated_data <- list()
  
  for(limited_model_to_simulate in vector_of_limited_models){
    
    name_of_limited_model_for_list <- ifelse(
      limited_model_to_simulate == FALSE,"regular", limited_model_to_simulate
    )
    
    list_of_simulated_data[[name_of_limited_model_for_list]] <- list()
    
    for(conf_transform_to_simulate in vector_of_conf_transform_options){
      
      # We get an indicator for the list of the data generated
      name_of_conf_transform_for_list <- ifelse(
        conf_transform_to_simulate, "w_conftrans", "wo_conftrans")
      
      for(i in 1:n_to_recover){
        
        list_of_simulated_data[[name_of_limited_model_for_list]][[name_of_conf_transform_for_list]][[i]] <- 
          vector(mode = "list", length = n_to_recover)
        
        # Grab the main learner_type <- note that this is always the full regular learner
        current_learner_type <- df_parameters_for_recovery[i,]
        current_learner_type$forgetting_type <- "noise"
        
        
        # make changes to this to take into account the limited model and confidence transformation
        
        # Conf Transform
        if(!conf_transform_to_simulate){
          current_learner_type$conf_transform <- 1 # disables conf transform
        }
        
        # Model parameters
        if(limited_model_to_simulate %in% c("one_prior", "one_prior_and_forgetting")){
          
          # Unify prior by overwritting old prior
          current_learner_type$beta_g_mean <- current_learner_type$beta_b_mean
          current_learner_type$beta_g_uncertainty <- current_learner_type$beta_b_uncertainty
          current_learner_type$alpha_g_prior <- current_learner_type$alpha_b_prior
          current_learner_type$beta_g_prior <- current_learner_type$beta_b_prior
          
          if(limited_model_to_simulate == "one_prior_and_forgetting"){
            current_learner_type$forgetting_type <- "forgetting_towards"
          }
        }
        
        list_of_simulated_data[[name_of_limited_model_for_list]][[name_of_conf_transform_for_list]][[i]] <-
          run_learner_on_game_with_several_sources(
            df_input_not_played =  list_of_raw_game_dfs[[i]],
            sources_to_run = sources_to_run,
            learner_type = current_learner_type,
            game_attributes = list_of_game_attributes[[i]],
            agent_name = i, 
            run_station_only_trials = run_station_only_trials,
            with_added_noise = TRUE)
        
        # Add information for fitting
        list_of_simulated_data[[name_of_limited_model_for_list]][[name_of_conf_transform_for_list]][[i]]$learner_type <-
          as.list(current_learner_type)
        
        list_of_simulated_data[[name_of_limited_model_for_list]][[name_of_conf_transform_for_list]][[i]]$game_attributes <- list_of_game_attributes[[i]]
      }
    }
  }
  
  return(list_of_simulated_data)
  
}