# File with functions that run the entire agent on  simulated 'game'
# File with functions that wrap around run_learner on game and the station only 
# trials to produce a final


# Function that runs learner and also returns the station-only trials
# Works for one source and returns a list with the main dataframe and the
# station only trials as a vector sorted by the X_F
run_learner_on_game_and_station_only <- function(
    df_game, 
    learner_type,
    game_attributes,
    agent_name = NA,
    resolution = 50,
    station_only = TRUE
){
  
  # Run the learner ------------------------------------------------------------
  run_learner_output <- run_learner_on_game(
    df_game = df_game, 
    learner_type = learner_type,
    game_attributes = game_attributes,
    agent_name = agent_name,
    resolution = 50,
    function_output = "df_game_and_learner_output"
  )
  
  if(station_only){
    station_only_trials <- get_station_only_trial_predictions(
      list_returned_from_run_learner = run_learner_output,
      game_attributes = game_attributes,
      learner_type = learner_type,
      resolution = resolution
    )
  }
  
  list_to_return <- list(df_game = run_learner_output$df_game)
  
  if(station_only) list_to_return$station_only_trials <- station_only_trials
  
  return(list_to_return)
  
}




# Function that plays with several sources instead of just one
run_learner_on_game_with_several_sources <-
  function(df_input_not_played,
           sources_to_run,
           learner_type,
           game_attributes,
           with_added_noise = FALSE,
           agent_name = NULL,
           run_station_only_trials = FALSE
  ){
    
    # Play with first source to set up the dataframe
    df_source_game_temp <- subset_game_df_by_source(df_input_not_played,
                                                    sources_to_run[1])
    run_learner_output <- run_learner_on_game_and_station_only(
      df_game = df_source_game_temp,
      learner_type = learner_type,
      game_attributes = game_attributes,
      agent_name = agent_name
    )
    
    # Save first dataframe which will later be appended to
    df_played_game <- run_learner_output$df_game
    
    # Save the station-only trials
    if(run_station_only_trials){
      station_only_trials_list <- list()
      station_only_trials_list[[sources_to_run[1]]] <-
        run_learner_output$station_only_trials
    }
    
    # Iterate over the rest
    for (i in 2:length(sources_to_run)) {
      
      current_source <- sources_to_run[i]
      
      df_source_game_temp <- subset_game_df_by_source(df_input_not_played,
                                                      current_source)
      
      run_learner_output <- 
        run_learner_on_game_and_station_only(
          df_game = df_source_game_temp,
          learner_type = learner_type,
          game_attributes = game_attributes,
          agent_name = agent_name
        )
      
      # Save first dataframe which will later be appended to
      df_source_game_temp <- run_learner_output$df_game
      
      # append this game
      df_played_game <- rbind(
        df_played_game,
        df_source_game_temp
      )
      
      if(run_station_only_trials){
        station_only_trials_list[[current_source]] <-
          run_learner_output$station_only_trials
      }
    }
    
    # print(station_only_trials_list)
    
    # Add noise
    if(with_added_noise){
      
      df_played_game <- add_noise_to_df(df_played_game,learner_type$confidence_noise)
      
      if(run_station_only_trials){
        station_only_trials_list <- 
          add_noise_to_station_only(station_only_trials_list,
                                    learner_type$confidence_noise)
      }
    }
    
    # print(station_only_trials_list)
    
    # Get output ready
    list_to_return <- list(df_played_game = df_played_game)
    if(run_station_only_trials){
      list_to_return$station_only_trials <- station_only_trials_list
    }
    
    return(list_to_return)
    
}
