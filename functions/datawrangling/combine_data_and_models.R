# File for applying the models to data

combine_model_and_data_pipeline <- function(df_main_task){
  
  list_of_game_dataframes <- 
    convert_data_df_to_game_df_for_modelling(df_main_task)
  
  list_of_model_predictions <-
    get_normative_model_predictions_for_list_of_game_dfs(list_of_game_dataframes)
  
  df_model_predictions <- 
    get_list_of_model_predictions_into_one_df(list_of_model_predictions)
  
  df_main_task <- combine_model_and_data_dataframes(df_data = df_main_task,
                                                    df_model_predictions,
                                                    list_of_model_predictions)
  
  df_main_task <- df_main_task %>% 
    # filter(! completion_code %in% participants_to_exclude$full) %>% 
    mutate(side_chosen = sign(response -50),
           model_side_chosen = sign(model_response - 50)) # green: 0, blue: 1
  
  return(df_main_task)
  
}




# Sets up a list with the attributes of the game to be played
set_up_default_game_attributes <- function(base_probability = 0.75,
                                           n_trials = 27,
                                           n_draws = 5,
                                           bias_high = .9,
                                           bias_low = .5,
                                           news_station_first = FALSE,
                                           with_independent_council) {
  
  return(
    list(n_trials = n_trials,     # number of trials
         n_draws = n_draws,        # number of balls to be drawn
         b_I = base_probability,     # INITIAL DRAW
         g_I = base_probability,
         b_F_helpful = base_probability,   # FINAL (SOURCE) DRAW: 
         g_F_helpful = base_probability,
         b_F_random = .5,
         g_F_random = .5,
         b_F_opposite = 1 - base_probability,
         g_F_opposite = 1 - base_probability,
         b_F_greenbias = bias_low,
         g_F_greenbias = bias_high,
         b_F_bluebias = bias_high,
         g_F_bluebias = bias_low,
         news_station_first = news_station_first,
         with_independent_council = with_independent_council)
  )
}





# Sets up a list with learner type which is entered into the model
set_up_default_learner_type_l <- function(beta_mean = 0.5 ,
                                          beta_uncertainty = 2,
                                          lambda_forgetting = 0,
                                          alpha_weight = 1,
                                          beta_weight = 1,
                                          alpha_forget_towards = 1,
                                          beta_forget_towards = 1,
                                          type,
                                          forgetting_type = "noise",
                                          conf_transform = 1,
                                          b_I_subjective = NA,
                                          g_I_subjective = NA,
                                          game_attributes = NA
                                         ) {
  
  learner_type_l <- list(
    alpha_b_prior = NA,
    beta_b_prior = NA,
    alpha_g_prior = NA,
    beta_g_prior = NA,
    beta_b_mean = beta_mean,
    beta_b_uncertainty = beta_uncertainty,
    beta_g_mean = beta_mean,
    beta_g_uncertainty = beta_uncertainty,
    lambda_forgetting = lambda_forgetting,
    alpha_weight = alpha_weight,
    beta_weight = beta_weight,
    alpha_forget_towards = alpha_forget_towards,
    beta_forget_towards = beta_forget_towards,
    type = type,
    forgetting_type = forgetting_type,
    conf_transform = conf_transform,
    b_I_subjective = b_I_subjective,
    g_I_subjective = g_I_subjective,
    variational = F
  )
  
  
  if(is.na(b_I_subjective) & is.na(g_I_subjective)){
    learner_type_l$b_I_subjective <- game_attributes$b_I
    learner_type_l$g_I_subjective <- game_attributes$g_I
  }
  
  return(learner_type_l)
}





# necessary for converting the data to a dataframe necessary for the model
change_state_string_to_integer = function(state_string){
  # change green and blue to -1 and 1
  return(as.integer(state_string == 'blue') * 2 - 1)
}

# takes one participants dataframe and returns the game dataframes that 
# serve as input to the modelling
convert_data_df_to_game_df_for_modelling <- function(df_data_all_participants){
  
  list_of_games <- list() # list with each source being an element
  
  completion_codes_in_data <- unique(df_data_all_participants$completion_code)
  
  
  for (current_participant in completion_codes_in_data) {
    
    df_one_participant <- df_data_all_participants %>% 
      filter(completion_code == current_participant)
    
    
    sources_played_with <- as.vector(unique(df_one_participant$source_type))
    
    list_of_games[[current_participant]]$feedback_condition <- df_one_participant$condition[1]
    list_of_games[[current_participant]]$news_station_first <- df_one_participant$news_station_first[1]
    list_of_games[[current_participant]]$sources_played_with <- sources_played_with
    
    
    for (source_type_i in sources_played_with) {
      
      list_of_games[[current_participant]][[source_type_i]] <- 
        df_one_participant %>% 
        filter(plugin_type == "initial_source",
               block_stage != "station_only",
               source_type == source_type_i) %>% 
        select(trial_number_within_block, 
               correct_state, 
               num_blue_indep, 
               num_blue_news) %>% 
        rename(trial = trial_number_within_block,
               ground_truth = correct_state,
               X_I = num_blue_indep,
               X_F = num_blue_news) %>% 
        mutate(ground_truth = change_state_string_to_integer(ground_truth))
    }
  }
  
  return(list_of_games)
}


# ----
get_normative_model_predictions_for_list_of_game_dfs <- function(list_of_game_dataframes,
                                                                 run_station_only = TRUE){
  # Takes a list_of_game_data_frames and returns a list of dfs that have the 
  # agent run
  
  list_of_model_predictions <- list()
    
  for (current_participant in names(list_of_game_dataframes)) {
    
    # identify the individual source types
    sources_of_current_participant <- list_of_game_dataframes[[current_participant]]$sources_played_with
    
    game_attributes_of_current_participant <- set_up_default_game_attributes(
      n_trials = nrow(list_of_game_dataframes[[current_participant]]$helpful),
      news_station_first = ifelse(
        list_of_game_dataframes[[current_participant]]$news_station_first == "First",
        TRUE, FALSE
      )
    )
    
    learner_type_of_current_participant <- set_up_default_learner_type_l(
      type = ifelse(
        list_of_game_dataframes[[current_participant]]$feedback_condition == "Without",
        "bayes_learner",
        "ground_truth"
      ),
      b_I_subjective = game_attributes_of_current_participant$b_I,
      g_I_subjective = game_attributes_of_current_participant$g_I
    )
    
    for (current_source in sources_of_current_participant){
      
      current_learner_output <-
        run_learner_on_game(
          df_game = list_of_game_dataframes[[current_participant]][[current_source]],
          learner_type = learner_type_of_current_participant,
          game_attributes = game_attributes_of_current_participant,
          agent_name = current_participant,
          function_output = "df_game_and_learner_output"
        )        
      
      list_of_model_predictions[[current_participant]][[current_source]] <- current_learner_output

      list_of_model_predictions[[current_participant]][[current_source]]$df_game$news_station_first <-
        list_of_game_dataframes[[current_participant]]$news_station_first
      
      if(run_station_only){
        
        list_of_model_predictions[[current_participant]][[current_source]]$station_only_predictions <-
          get_station_only_trial_predictions(
            list_returned_from_run_learner = current_learner_output,
            game_attributes = game_attributes_of_current_participant,
            learner_type = learner_type_of_current_participant
          )
      }
    }
  }
  
  return(list_of_model_predictions)
  
}


get_list_of_model_predictions_into_one_df <- function(list_of_model_predictions){
  
  # set up empty dataframe
  df_model_predictions <- list_of_model_predictions[[1]]$helpful$df_game
  df_model_predictions$source_type <- "placeholder"
  df_model_predictions <- df_model_predictions[0,]
  
  for(current_participant in names(list_of_model_predictions)){
    
    for(current_source in names(list_of_model_predictions[[current_participant]])){
      
      df_to_concatenate <- 
        list_of_model_predictions[[current_participant]][[current_source]]$df_game
      df_to_concatenate$source_type <- current_source
      
      # print(df_to_concatenate)
      
      df_model_predictions <- bind_rows(
        df_model_predictions,
        df_to_concatenate
      )
    }
  }
  
  return(df_model_predictions)
}

### ----
# get_model_predictions_into_dataframe <- function(df_data,
#                                                  list_of_model_predictions){
#   
#   # we first define an empty dataframe with some dummy attributes
#   game_attributes <- set_up_default_game_attributes(
#     n_trials = nrow(list_of_game_dataframes[[1]]$helpful)
#   )
#   
#   learner_type_l <- set_up_default_learner_type_l(
#     type = "bayes_learner",
#     b_I_subjective = game_attributes$b_I,
#     g_I_subjective = game_attributes$g_I
#   )
#   
#   df_model_predictions <- run_learner_on_game(
#     df_game = list_of_game_dataframes[[1]][[1]],
#     learner_type = learner_type_l,
#     game_attributes = game_attributes,
#     agent_name = "set-up")
#   df_model_predictions$source_type = NA
#   df_model_predictions <- df_model_predictions[0, ] 
# 
#   
#   # Then we iterate over every participant in the data
#   completion_codes_in_data <- unique(df_data$completion_code)
#   
#   # move the dataframes from the list into the dataframe
#   for (current_participant in completion_codes_in_data) {
#     
#     df_one_participant <- df_data %>% 
#       filter(completion_code == current_participant)
#     
#     sources_of_current_participant <- as.vector(unique(df_one_participant$source_type))
#     
#     for (current_source in sources_of_current_participant){
#       
#       current_model_output_df <- 
#         list_of_model_predictions[[current_participant]][[current_source]]$df_game
#       
#       current_model_output_df$source_type = current_source
#       
#       df_model_predictions <- rbind(
#         df_model_predictions,
#         current_model_output_df
#       )
#     }
#   }
#   
#   df_model_predictions$source_type <- 
#     factor(df_model_predictions$source_type,      
#            levels = c("helpful",
#                       "random",
#                       "opposite",
#                       "greenbias",
#                       "bluebias"))
#   
#   return(df_model_predictions)
# }


### ---
combine_model_and_data_dataframes <- function(df_data,
                                              df_model_predictions,
                                              list_of_model_predictions) {
  
  # Set the two confidences in relation to blue -> this is necessary to
  # align with the "response" variable in the empirical data
  df_model_predictions <- df_model_predictions %>% 
    mutate(c_I_in_blue = (1 - c_I)*(a_I == -1) + c_I*(a_I == 1),
           c_F_in_blue = (1 - c_F)*(a_F == -1) + c_F*(a_F == 1))
  
  df_model_predictions_long <- df_model_predictions %>% 
    gather(key = "trial_stage",
           value = "model_response",
           c("c_I_in_blue", "c_F_in_blue"))
  
  # df_model_predictions_long
  
  df_data$model_response <- NA
  
  completion_codes_in_data <- unique(df_data$completion_code)
  
  for (current_participant in completion_codes_in_data) {
    
    # grab which sources were played with
    df_one_participant <- df_data %>% 
      filter(completion_code == current_participant)
    sources_of_current_participant <- as.vector(unique(df_one_participant$source_type))
    
    for (current_source in sources_of_current_participant){
      
      for (current_response in c("initial_source", "full_source")) {
        
        # add main trials
        response_filter_criterion = ifelse(current_response == "initial_source",
                                           "c_I_in_blue", "c_F_in_blue")
        
        responses_to_add <- df_model_predictions_long %>%   
          filter(agent_name == current_participant,
                 source_type == current_source,
                 trial_stage == response_filter_criterion) %>% 
          select(model_response)
        
        df_data[which(
          df_data$completion_code == current_participant &
            df_data$source_type == current_source &
            df_data$block_stage != "station_only" &
            df_data$plugin_type == current_response
        ),]$model_response <- responses_to_add$model_response
        
      }
      
      # # Add station only trials
      # Get that posterior
      station_only_vector <-
        list_of_model_predictions[[current_participant]][[current_source]]$station_only_predictions
      
      # print(station_only_vector)
      
      # Needs to be reordered because the actual participants are not presented 
      # with the news source from 0 to n_draws but rather in a randomized fashion
      num_blue_order_vector <- df_data[which(
        df_data$completion_code == current_participant &
          df_data$source_type == current_source &
          df_data$block_stage == "station_only"),
      ]$num_blue_news
      
    
      # Actually add data
      df_data[which(
        df_data$completion_code == current_participant &
          df_data$source_type == current_source &
          df_data$block_stage == "station_only"),
      ]$model_response <- station_only_vector[num_blue_order_vector + 1]
      
    }
  }
  df_data$model_response <- 100 * df_data$model_response
  
  df_data <- df_data %>% 
    mutate(model_response_absolute = (abs(model_response - .5) + .5),
           model_side_chosen = sign(model_response - 50))
 
  return(df_data)
}
