# Main function for running the learner on a single block

run_learner_on_game <- function(
    df_game, # df of a game block - contains 'stimuli'   
    learner_type, # the attributes of the learner (i.e. parameters)
    game_attributes, # the attributes of the game (e.g. n_trials)
    agent_name = NA,
    resolution = 50,
    function_output = "df_game" # can either output a df or a list with more info
    ){
  # Function that takes a dataframe of a game and runs a specific agent on it,
  # returning the df with the agent information
  
  # Inputs:
  # - df_game: dataframe with game (for example created from get_game())
  # - learner_type (list containing e.g. priors or whether learning with or 
  #   without feedback) 
  # - game_attributes (list containing the attributes of the currently played game)
  # - resolution: resolution of numerical integration
  # - output: whether to return the dataframe only or additional information
  
  # We tag the data frame for further processing
  df_game$agent_name <- agent_name
  df_game$learner_type <- learner_type$type

  # Sanity check on conditions
  if (!game_attributes$with_independent_council &&  learner_type$type == "bayes_learner")  {
    stop("Can't run Bayes Learner without Independent Council")
  }
    
  # In the following, we run the different subparts of the learner. Because this
  # sometimes relies on vectorized representations of the trials, we do the
  # individual operations (e.g. compute action) on the full stimulus vector
  # instead of on a trial by trial basis
  
  # 1. Get the initial decisions and confidences -------------------------------
  
  # This is applicable to both the forward and backward version
  # but not the version where we don't have an independent council
  if (game_attributes$with_independent_council) {
    log_odds_a_I_c_I_independent_council <- 
      get_logodds_a_I_and_c_I(df_game$X_I,
                              game_attributes$n_draws,
                              game_attributes$b_I)
    logodds_blue_is_vector <- log_odds_a_I_c_I_independent_council$logodds_blue
  }
  
  # 2. Run the learner: Learning about the source ------------------------------ 
  
  # This is applicable a priori to both the news station first and second versions
  # Note that the learner output then contains the belief states   over the
  # 'usefulness' of the sources which is then used further below for the
  # trial-by-trial belief update
  if (learner_type$type == "ground_truth") {
    
    learner_output <- 
      run_ground_truth_learner(ground_truth = df_game$ground_truth,
                               X_F = df_game$X_F,
                               learner_type = learner_type,
                               n_draws = game_attributes$n_draws)
    
  } else if (learner_type$type == "bayes_learner"){
    
    learner_output <- 
      run_bayesian_learner(X_I = df_game$X_I,
                           X_F = df_game$X_F,
                           game_attributes = game_attributes,
                           resolution = resolution,
                           learner_type = learner_type)
  }
  
  # 3. Assign the decisions ----------------------------------------------------
  
  if(game_attributes$news_station_first == FALSE){
    # note how this also means that there must be an independent council
    
    # Initial decision
    a_Is_vector <- log_odds_a_I_c_I_independent_council$a_i
    c_Is_vector <- log_odds_a_I_c_I_independent_council$c_i
    posteriors_blue_I_vector <- log_odds_a_I_c_I_independent_council$posterior_blue
    
    
    # Run and save the belief update within each trial
    posteriors_blue_F_vector <- 
      run_within_trial_belief_update(X_I_v = df_game$X_I,
                                     X_F_v = df_game$X_F,
                                     learner_type = learner_type,
                                     learner_output = learner_output,
                                     game_attributes = game_attributes,
                                     resolution = resolution)
    
    # Final decision
    a_Fs_vector <- sign(posteriors_blue_F_vector - .5)
    c_Fs_vector <- posteriors_blue_F_vector
    c_Fs_vector[a_Fs_vector == -1] = 1 - c_Fs_vector[a_Fs_vector == -1]
    
  } else if(game_attributes$news_station_first == TRUE) {
    
    # Initial decision
    # The posteriors are updated from a flat prior
    # (because of setting in the game_attributes list)
    posteriors_blue_I_vector <- 
      run_within_trial_belief_update(X_I_v = df_game$X_I,
                                     X_F_v = df_game$X_F,
                                     learner_type = learner_type,
                                     learner_output = learner_output,
                                     game_attributes = game_attributes,
                                     resolution = resolution)
    
    a_Is_vector <- sign(posteriors_blue_I_vector - .5)
    c_Is_vector <- posteriors_blue_I_vector
    c_Is_vector[a_Is_vector == -1] = 1 - c_Is_vector[a_Is_vector == -1]
    
    # Final decision
    if (game_attributes$with_independent_council) {
      posteriors_blue_F_vector <- run_news_station_first_within_belief_update(
        log_odds_a_I_c_I_independent_council, posteriors_blue_I_vector)
      a_Fs_vector <- sign(posteriors_blue_F_vector - .5)
      c_Fs_vector <- posteriors_blue_F_vector
      c_Fs_vector[a_Fs_vector == -1] = 1 - c_Fs_vector[a_Fs_vector == -1]
    }
  }
  
  # Add the scaling of the confidence (this is done when saving) ---------------
  if(!"conf_transform" %in% names(learner_type)){
    learner_type$conf_transform <- 1
  } 

  # Save attributes ------------------------------------------------------------
  
  df_game$b_F_estimate <- learner_output$mean_estimate_b_F
  df_game$g_F_estimate <- learner_output$mean_estimate_g_F
  
  df_game$posterior_blue_I <- 
    transform_conf(posteriors_blue_I_vector, learner_type$conf_transform)
  df_game$a_I <- a_Is_vector
  df_game$c_I <- transform_conf(c_Is_vector, learner_type$conf_transform)
  
  if (game_attributes$with_independent_council) {
    df_game$posterior_blue_F <- 
      transform_conf(posteriors_blue_F_vector, learner_type$conf_transform)
    df_game$a_F <- a_Fs_vector
    df_game$c_F <- transform_conf(c_Fs_vector, learner_type$conf_transform)
    
    df_game$posterior_indep <- log_odds_a_I_c_I_independent_council$posterior_blue
    df_game$logodds_blue_I <- logodds_blue_is_vector
  } else {
    df_game$posterior_blue_F <- NA
    df_game$a_F <- NA
    df_game$c_F <- NA
    df_game$posterior_indep <- NA
    df_game$logodds_blue_I <- NA
  }
  
  # Output ---------------------------------------------------------------------
  
  if (function_output == "df_game") {
    return(df_game)
  } else if (function_output == "df_game_and_learner_output"){
    
    return(
      list(
        agent_name = agent_name,
        df_game = df_game, 
        learner_output = learner_output
      )
    )
  } else {
    warning("function_ouput not specified in run_learner_on_game")
  }
}