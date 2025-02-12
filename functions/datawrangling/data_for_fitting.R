# Functions that allow the combination of data and models

## Fitting Formatting ##############################################################

get_df_ML_parameters_from_list_of_fits <- function(
    list_of_fits,
    feedback_condition = "bayes_learner",
    parameters_not_to_fit = c(),
    limited_model = FALSE,
    conf_transform = FALSE){
  
  
  # First, get names of parameters
  parameters_fit <- get_vector_of_parameters_to_fit(
    feedback_condition = feedback_condition,
    how_to_fit_beta = "mean_uncertainty",
    parameters_not_to_fit = parameters_not_to_fit,
    limited_model = limited_model,
    conf_transform = conf_transform)
  
  ##  Dataframe for max likelihood estimate ---- 
  if(!is.null(names(list_of_fits))){ # in case of real data with actual completion codes
    participants_to_fit <- names(list_of_fits)
  } else {
    participants_to_fit <- 1:length(list_of_fits)
  }

  num_of_iterations <- list_of_fits[[1]]$optim$iter # this assumes all participants have same
  
  df_ML_fit_parameters <-
    data.frame(matrix(ncol = length(parameters_fit) + 1, 
                      nrow = length(participants_to_fit)))
  colnames(df_ML_fit_parameters) <- c("completion_code", parameters_fit)
  
  df_ML_fit_parameters$completion_code <- participants_to_fit
  
  for(i in 1:length(participants_to_fit)){
    
    current_participant <- participants_to_fit[i]
    
    current_bestem <- 
      list_of_fits[[current_participant]]$optim$bestmem
    
    df_ML_fit_parameters[df_ML_fit_parameters$completion_code == current_participant,
                         2:ncol(df_ML_fit_parameters)] <- current_bestem
    
  }
  
  print(df_ML_fit_parameters)
  
  # Make sure that both with and without feedback parameters are represented
  if (limited_model != "one_prior_and_forgetting" & !"alpha_weight" %in% colnames(df_ML_fit_parameters)) {
    df_ML_fit_parameters %<>% 
      mutate(alpha_weight = alpha_forget_towards,
             beta_weight = beta_forget_towards)
  }

  
  ## add neg_log_likelihood
  df_ML_fit_parameters$neg_log_likelihood <- NA
  
  for(i in 1:length(participants_to_fit)){
    
    current_participant <- participants_to_fit[i]
    
    current_neg_log_lik <- 
      list_of_fits[[current_participant]]$member$bestvalit[[num_of_iterations]]
    
    df_ML_fit_parameters[df_ML_fit_parameters$completion_code == current_participant,]$neg_log_likelihood <- 
      current_neg_log_lik
    
  }
  

  return(df_ML_fit_parameters)
  
}






get_df_likelihood_and_param_evolution <- function(
    list_of_fits,
    feedback_condition = "bayes_learner",
    parameters_not_to_fit = c(),
    limited_model = FALSE,
    conf_transform = FALSE){
  
  # First, get names of parameters
  parameters_fit <- get_vector_of_parameters_to_fit(
    feedback_condition = feedback_condition,
    how_to_fit_beta = "mean_uncertainty",
    parameters_not_to_fit = parameters_not_to_fit,
    limited_model = limited_model,
    conf_transform = conf_transform)
  
  ## Number of iterations and individual iteration best estimates -----
  
  num_of_iterations <- list_of_fits[[1]]$optim$iter # this assumes all participants have same
  if(!is.null(names(list_of_fits))){ # in case of real data with actual completion codes
    participants_to_fit <- names(list_of_fits)
  } else {
    participants_to_fit <- 1:length(list_of_fits)
  }
  
  
  # set up basic df
  df_likelihood_and_parameter_evolution <- 
    data.frame(matrix(ncol = length(parameters_fit) + 3, 
                      nrow = length(participants_to_fit) * num_of_iterations))
  
  colnames(df_likelihood_and_parameter_evolution) <-
    c("completion_code", "iteration", "neg_log_likelihood", parameters_fit)
  
  df_likelihood_and_parameter_evolution %<>% 
    mutate(
      completion_code = rep(participants_to_fit, each = num_of_iterations),
      iteration = rep(1:num_of_iterations, length(participants_to_fit))
    )
  
  for(i in 1:length(participants_to_fit)){
    
    current_participant <- participants_to_fit[i]
    
    current_bestvalit <- list_of_fits[[i]]$member$bestvalit
    current_betmemit <- list_of_fits[[i]]$member$bestmemit
    
    df_likelihood_and_parameter_evolution[
      df_likelihood_and_parameter_evolution$completion_code == current_participant,
      3:ncol(df_likelihood_and_parameter_evolution)] <- 
      cbind(current_bestvalit, current_betmemit)
  }
  
  return(df_likelihood_and_parameter_evolution)
}


## DATA -> Fitting ##############################################################


# Functions that help with getting data ready for fitting

convert_data_df_into_simulation_like_df <- function(df_main_task,
                                                    get_rts = FALSE){
  
  if(get_rts){
    columns_to_pivot_wide <- c("response", "rt")
  } else {
    columns_to_pivot_wide <- "response"
  }
  
  # First, we get the data into the right format
  df_main_task_wide <- df_main_task %>% 
    select(
      trial_number_within_block,
      completion_code, 
      condition, 
      news_station_first,
      with_independent_council,
      response, 
      plugin_type,
      num_blue_indep,
      num_blue_news,
      source_type,
      block_stage,
      block_half,
      block_quarter,
      correct_state,
      rt
    ) %>% 
    mutate(
      response = response / 100
    ) %>% 
    pivot_wider(
      id_cols = c(
        "trial_number_within_block",
        "completion_code", 
        "condition", 
        "news_station_first",
        "with_independent_council",
        "num_blue_indep",
        "num_blue_news",
        "source_type",
        "block_stage",
        "block_half",
        "block_quarter",
        "correct_state"
      ), 
      names_from = "plugin_type", 
      values_from = columns_to_pivot_wide
    ) %>% 
    rename(
      X_I = num_blue_indep,
      X_F = num_blue_news
    )
  
  if(get_rts){
    df_main_task_wide %<>%
      rename(posterior_blue_I = response_initial_source) %>% 
      mutate(rt_initial_source = as.numeric(rt_initial_source))
    
    if(df_main_task$with_independent_council[1] == "true"){
      df_main_task_wide %<>%
        rename(posterior_blue_F = full_source) %>% 
        mutate(rt_full_source = as.numeric(rt_full_source))
    }
  } else {
    df_main_task_wide %<>%
      rename(posterior_blue_I = initial_source)
  
    if(df_main_task$with_independent_council == "true"){
      df_main_task_wide %<>%
        rename(posterior_blue_F = full_source)
    }
  }
  
  df_main_task_wide %<>%
    mutate(ground_truth = ifelse(correct_state == "blue", 1, -1))
  
  if(df_main_task$with_independent_council[1] == "true"){
    df_main_task_wide %<>%
      mutate(change_of_mind = posterior_blue_F - posterior_blue_I)
  }

  return(df_main_task_wide)
  
}

get_data_into_format_for_fitting <- function(df_main_task){
  # Takes empiricial data from online task and produces (in data_for_fitting 
  # list on a per participant basis)
  #   - a 'wide' format df of the main trials "df_main_trials" ready to be entered in
  #     the fitting pipeline
  #   - a 'wide' format df of the station only trials "df_station_only"
  #   - learner_type and game_attributes for the simulations
  
  
  # First, we get the data into the right format
  df_main_task_wide <- convert_data_df_into_simulation_like_df(df_main_task)

  participants_to_loop_over <- unique(df_main_task_wide$completion_code)
  
  data_for_fitting <- list()
  
  for(current_participant in participants_to_loop_over){
    
    # Start by generating a list
    data_for_fitting[[current_participant]] <- list()
    
    # Filter participant down to one participant
    df_one_participant <- df_main_task_wide %>% 
      filter(completion_code == current_participant)
    
    # Grab sources that the participant played with
    sources_played_with <- as.vector(unique(df_one_participant$source_type))
    
    data_for_fitting[[current_participant]]$df_main_trials_all_sources <-
      df_one_participant[FALSE,]
    
    data_for_fitting[[current_participant]][["station_only_trials"]] <- list()
    
    for (i in 1:length(sources_played_with)) {
      
      current_source <- sources_played_with[i]
      
      df_one_participant_one_source <- df_one_participant %>% 
        filter(source_type == current_source,
               block_stage == "normal")
      
      data_for_fitting[[current_participant]]$df_main_trials_all_sources <-
        bind_rows(
          data_for_fitting[[current_participant]]$df_main_trials_all_sources,
          df_one_participant_one_source
        )
      
      df_station_only <- df_one_participant %>%
        filter(block_stage == "station_only",
               source_type  == current_source) %>% 
        arrange(X_F) # already order by X_F to always have it in the same order
      
      if(df_main_task$with_independent_council[1] == "false"){
        df_station_only$posterior_blue_F <- df_station_only$posterior_blue_I
      } else if (df_main_task$with_independent_council[1] == "true" & is.na(df_station_only$posterior_blue_F[1])){
        # the new versions saves the responses under posterior_blue_I and leave _F empty
        df_station_only$posterior_blue_F <- df_station_only$posterior_blue_I
      } 
      
      data_for_fitting[[current_participant]][["station_only_trials"]][[current_source]] <-
        df_station_only$posterior_blue_F
      
      # Add game attributes and learner type
      if(i == 1){  
        # if statement because this only needs be done once - but we need it
        # here because of the num trials that comes out of the subset
        
        # Get and save game attributes
        if("with_independent_council" %in% names(df_one_participant)){
          current_with_independent_council <- df_one_participant$with_independent_council[1] == "true"
        } else {
          # old data doesn't contain this column and we therefore need to manually
          # define this
          current_with_independent_council <- TRUE
        }

        current_game_attributes <- set_up_default_game_attributes(
          n_trials = nrow(df_one_participant_one_source),
          news_station_first = df_one_participant$news_station_first[1] == "First",
          with_independent_council = current_with_independent_council
        )
                
        data_for_fitting[[current_participant]][["game_attributes"]] <- current_game_attributes
        
        # Get and save learner type
        data_for_fitting[[current_participant]][["learner_type"]] <- 
          set_up_default_learner_type_l(
            type = ifelse(df_one_participant$condition[1] == "Without",
                          "bayes_learner",
                          "ground_truth"),
            game_attributes = current_game_attributes # to pass down subjectives b_I/g_I
          )
      }
    } 
  }
  
  return(data_for_fitting)
  
}