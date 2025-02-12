set_up_regression_betas_data_frame <- function(participants_to_run_betas_on,
                                               sources_to_investigate){
  # Sets up the empty dataframe for computing main regressions
  
  # print(participants_to_run_betas_on)
  # print(sources_to_investigate)
  
  df_betas <- data.frame(
    completion_code = rep(participants_to_run_betas_on, each = length(sources_to_investigate)),
    source_type = sources_to_investigate,
    
    intercept_initial_confidence = NA,
    intercept_initial_confidence_sd = NA,
    slope_initial_confidence = NA,
    slope_initial_confidence_sd = NA,
    
    intercept_com = NA,
    intercept_com_sd = NA,
    slope_com = NA,
    slope_com_sd = NA,
    
    intercept_source_only = NA,
    intercept_source_only_sd = NA,
    slope_source_only = NA,
    slope_source_only_sd = NA,
    
    intercept_final = NA,
    intercept_final_sd = NA,
    slope_indep = NA,
    slope_indep_sd = NA,
    slope_news = NA,
    slope_news_sd = NA
    
    # slope_news_with_conf = NA,
    # slope_news_with_conf_sd = NA,
    # slope_initial_conf_to_final_conf = NA,
    # slope_initial_conf_to_final_conf_sd = NA 
  )
  
  # These come with the issues of correlation between IC and News Station
  # slope_indep_only = NA,
  # slope_indep_only_sd = NA,
  # slope_news_only = NA,
  # slope_news_only_sd = NA) # ,
  
  # model_slope_com = NA,
  # model_slope_com_sd = NA,
  # model_source_only = NA,
  # model_source_only_sd = NA)
  
  return(df_betas)
  
}
 
get_regression_betas <- function(
  df_main_task_wide,
  sources_to_investigate = c("helpful", "random", "opposite", "bluebias"),
  get_betas_for_model = FALSE,
  only_look_at_second_half = TRUE){
  # Get individiual regression parameters per participant
  # Computes:
  # - Regression for changes of mind
  # - Regression for initial confidence
  # - Regression for source only trials
  
  participants_to_run_betas_on <- as.vector(unique(df_main_task_wide$completion_code))
  
  # create empty dataframe
  df_betas <- set_up_regression_betas_data_frame(participants_to_run_betas_on,
                                                 sources_to_investigate)
  
  
  # For all participants: Enter the initial confidence as a lagged confidence 
  # and transform the response to a 0-1 scale
  # df_main_task <- df_main_task %>% 
  #   mutate(response = response / 100) %>% 
  #   mutate(response_lagged = lag(response))
  
  # For output
  n_total_regressions <- length(participants_to_run_betas_on) * length(sources_to_investigate) 
  index_length <- round(log10(n_total_regressions)) + 1
  printed_index <- 0
  
  for(current_participant in participants_to_run_betas_on) {
    
    # Choose the sources (e.g. if one participant didn't have a source saved )
    df_one_participant <- filter(df_main_task_wide, completion_code == current_participant)
    possible_sources <- as.vector(unique(df_one_participant$source_type))
    
    for(current_source in sources_to_investigate) {
      
      cat(
        paste0(
          format(printed_index, width = index_length),
                " / ", n_total_regressions, " | ",
                sep = "")
        )
      printed_index <- printed_index + 1
      
      if(current_source %in% possible_sources){
        
        df_one_participant_one_source <- filter(
          df_one_participant, 
          source_type == current_source
          )
        
        # selection vector for adding to df
        select_vector <- 
          df_betas$completion_code == current_participant &
          df_betas$source_type == current_source
        
        df_one_participant_main_trials <- df_one_participant_one_source %>% 
          filter(block_stage == "normal",
                 block_half %in% ifelse(only_look_at_second_half, 2, c(1,2)))
        
        ## Changes of mind and initial confidence ---
        # both depend on news station first
        
        
        # getting the slope for the real data
        if(df_one_participant$news_station_first[1] == "First"){
          # independent council is shown second so modulates changes of mind 
          
          if(df_one_participant$with_independent_council[1] == 'true'){
            
            m_com_by_blue_final <- summary(lm(change_of_mind ~ I(X_I-2.5), 
                                              df_one_participant_main_trials))
          }

          
          m_initial_confidence <- summary(
            bayesglm(posterior_blue_I ~  I(X_F-2.5), 
                     data = df_one_participant_main_trials,
                     family = "binomial"))
        } else if(df_one_participant$news_station_first[1] == "Second"){
          
          m_com_by_blue_final <- summary(lm(change_of_mind ~ I(X_F-2.5), 
                                            df_one_participant_main_trials))


          m_initial_confidence <- summary(
            bayesglm(posterior_blue_I ~  I(X_I-2.5), 
                     data = df_one_participant_main_trials,
                     family = "binomial")
            )
        }
        
        # only save Change of Mind when there is an independent council
        if (df_one_participant$with_independent_council == "true") {
          df_betas[select_vector,]$intercept_com <- 
            m_com_by_blue_final$coefficients[1,1]
          df_betas[select_vector,]$intercept_com_sd <- 
            m_com_by_blue_final$coefficients[1,2]
          df_betas[select_vector,]$slope_com <- 
            m_com_by_blue_final$coefficients[2,1]
          df_betas[select_vector,]$slope_com_sd <- 
            m_com_by_blue_final$coefficients[2,2]
        }
        
        df_betas[select_vector,]$intercept_initial_confidence <- 
          m_initial_confidence$coefficients[1,1]
        df_betas[select_vector,]$intercept_initial_confidence_sd <- 
          m_initial_confidence$coefficients[1,2]
        df_betas[select_vector, ]$slope_initial_confidence <-
          m_initial_confidence$coefficients[2,1]
        df_betas[select_vector, ]$slope_initial_confidence_sd <-
          m_initial_confidence$coefficients[2,2]
        
        
        
        ## Source_only trials ----
        
        df_source_only_one_participant <- df_one_participant_one_source %>% 
          filter(block_stage == "station_only")
        
  
        if(df_one_participant_one_source$with_independent_council[1] == "false"){
          # we fill in the data for the actual data only (the model already put the predictions in the right column)
          if(!"posterior_blue_F" %in% colnames(df_source_only_one_participant)){
            df_source_only_one_participant$posterior_blue_F = df_source_only_one_participant$posterior_blue_I
          }
          # for a joint with/without feedback dataset, we still have to paste them over
          if(!is.na(df_source_only_one_participant$posterior_blue_I[1])){
            df_source_only_one_participant$posterior_blue_F = df_source_only_one_participant$posterior_blue_I 
          }

        }
        if(df_one_participant_one_source$with_independent_council[1] == "true"){
          if(!is.na(df_source_only_one_participant$posterior_blue_I[1])){
            df_source_only_one_participant$posterior_blue_F = df_source_only_one_participant$posterior_blue_I 
          }
        }
        
        # using bayesglm based on: 
        # https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
        
        m_conf_source_only <- summary(bayesglm(posterior_blue_F ~  I(X_F-2.5), 
                                               data = df_source_only_one_participant,
                                               family = "binomial"))
        
        df_betas[select_vector,]$intercept_source_only <- 
          m_conf_source_only$coefficients[1,1]
        df_betas[select_vector,]$intercept_source_only_sd <- 
          m_conf_source_only$coefficients[1,2]
        df_betas[select_vector, ]$slope_source_only <-
          m_conf_source_only$coefficients[2,1]
        df_betas[select_vector, ]$slope_source_only_sd <-
          m_conf_source_only$coefficients[2,2]
        
        
        if(get_betas_for_model){
          m_model_com_by_blue_final <- summary(lm(model_change_of_mind ~ I(X_F-2.5), 
                                                  df_change_of_mind_participant))
          
          df_betas[select_vector,]$model_slope_com <- 
            m_model_com_by_blue_final$coefficients[2,1]
          df_betas[select_vector,]$model_slope_com_sd <- 
            m_model_com_by_blue_final$coefficients[2,2]
          
          
          df_source_only_one_participant <- df_source_only_one_participant %>% 
            mutate(model_response = model_response / 100)
          
          m_model_conf_source_only <- summary(bayesglm(model_response ~ X_F, 
                                                       data = df_source_only_one_participant,
                                                       family = "binomial"))
          
          df_betas[select_vector, ]$model_source_only <-
            m_model_conf_source_only$coefficients[2,1]
          df_betas[select_vector, ]$model_source_only_sd <-
            m_model_conf_source_only$coefficients[2,2]
        }
        
        
        ## Final confidence with the two stations as predictor ----
        

        if (df_one_participant$with_independent_council == "true") {
          m_final_confidence <- 
            summary(bayesglm(
              posterior_blue_F ~  I(X_I-2.5) + I(X_F-2.5), 
              data = df_one_participant_main_trials,
              family = "binomial")
            )
          
          df_betas[select_vector,]$intercept_final <- m_final_confidence$coefficients[1,1]
          df_betas[select_vector,]$intercept_final_sd <- m_final_confidence$coefficients[1,2]
          df_betas[select_vector,]$slope_indep <- m_final_confidence$coefficients[2,1]
          df_betas[select_vector,]$slope_indep_sd  <- m_final_confidence$coefficients[2,2]
          df_betas[select_vector,]$slope_news <- m_final_confidence$coefficients[3,1]
          df_betas[select_vector,]$slope_news_sd <- m_final_confidence$coefficients[3,2]
          
          m_final_confidence <- 
            summary(bayesglm(
              posterior_blue_F ~ I(X_I-2.5) +  I(X_F-2.5), 
              data = df_one_participant_main_trials,
              family = "binomial")
            )
        }
  
      }
    }
  }
  
  df_betas$source_type <- get_sources_in_correct_order(df_betas$source_type)
  
  return(df_betas)
  
}


################################################################################

get_regression_betas_split_by_half <- function(df_main_task_wide){
  
  participants_to_run_betas_on <- as.vector(unique(df_main_task_wide$completion_code))
  sources_to_investigate = c("helpful", "random", "opposite", "bluebias")
  
  df_beta_over_time <- data.frame(
    completion_code = rep(participants_to_run_betas_on,
                          each = length(sources_to_investigate) * 2),
    source_type = rep(sources_to_investigate),
    block_half = rep(1:2, each = 4), 
    slope = NA,
    slope_sem = NA,
    intercept = NA,
    intercept_sem = NA
  )
  
  df_beta_over_time$source_type <-
    get_sources_in_correct_order(df_beta_over_time$source_type)
  
  for(i in 1:nrow(df_beta_over_time)){
    
    cat(paste(format(i, width = 3),
              " / ", nrow(df_beta_over_time), " | ",
              sep = ""))
    
    curr_completion_code <- df_beta_over_time$completion_code[i]
    
    df_initial_conf_source_blockpart <- df_main_task_wide %>% 
      filter(
        completion_code == df_beta_over_time$completion_code[i],
        block_stage != "station_only",
        source_type == df_beta_over_time$source_type[i],
        news_station_first == "First",
        block_half == df_beta_over_time$block_half[i]
      ) 
    
    m_initial_conf_by_news_station <- summary(
      bayesglm(posterior_blue_I ~ I(X_F - 2.5), 
               data = df_initial_conf_source_blockpart,
               family = "binomial"))
    
    df_beta_over_time$intercept[i] <- m_initial_conf_by_news_station$coefficients[1,1]
    df_beta_over_time$intercept_sem[i] <- m_initial_conf_by_news_station$coefficients[1,2]
    df_beta_over_time$slope[i] <- m_initial_conf_by_news_station$coefficients[2,1]
    df_beta_over_time$slope_sem[i] <- m_initial_conf_by_news_station$coefficients[2,2]
  }
  
  return(df_beta_over_time)
  
}

################################################################################


get_betas_for_all_participants_split_by_block_quarter <- function(
    df_main_task_wide,
    with_independent_council = TRUE){
  # Returns a dataframe (df_beta_over_time_pooled) with betas of interest 
  # (change of mind/intitial confidence for news station second/first) for 
  # all participants in a pooled fashion (necssary because I need more data)
  
  participants_to_run_betas_on <- as.vector(unique(df_main_task$completion_code))
  sources_to_investigate <- c("helpful", "random", "opposite", "bluebias")
  news_station_first_column_set_up <- rep(unique(df_main_task_wide$news_station_first), each = 4 * length(sources_to_investigate))
  
  df_beta_over_time_pooled <- data.frame(
    # we don't do this per participant but per news station and block quarter
    # so set up the relevant dataframe here
    with_independent_council = with_independent_council,
    news_station_first = news_station_first_column_set_up,
    source_type = rep(sources_to_investigate),
    block_quarter = rep(1:4, each = 4), 
    slope = NA,
    slope_sem = NA
  )
  
  df_beta_over_time_pooled$source_type <- get_sources_in_correct_order(df_beta_over_time_pooled$source_type)
  
  for(i in 1:nrow(df_beta_over_time_pooled)){
    
    if(df_beta_over_time_pooled$news_station_first[i] == "Second"){
      
      df_com_sub_source_blockpart <- df_main_task_wide %>% 
        filter(
          block_stage != "station_only",
          source_type == df_beta_over_time_pooled$source_type[i],
          news_station_first == "Second",
          block_quarter == df_beta_over_time_pooled$block_quarter[i]
        )
      
      m_com_by_blue_final <- summary(
        lm(change_of_mind ~ I(X_F-2.5), df_com_sub_source_blockpart))
      
      df_beta_over_time_pooled$slope[i] <- m_com_by_blue_final$coefficients[2,1]
      df_beta_over_time_pooled$slope_sem[i] <- m_com_by_blue_final$coefficients[2,2]
      df_beta_over_time_pooled$intercept[i] <- m_com_by_blue_final$coefficients[1,1]
      df_beta_over_time_pooled$intercept_sem[i] <- m_com_by_blue_final$coefficients[1,2]
      
    } else if(df_beta_over_time_pooled$news_station_first[i] == "First"){
      df_initial_conf_source_blockpart <- df_main_task_wide %>% 
        filter(
          block_stage != "station_only",
          source_type == df_beta_over_time_pooled$source_type[i],
          news_station_first == "First",
          block_quarter == df_beta_over_time_pooled$block_quarter[i]
        )
      
      m_initial_conf_by_news_station <- summary(
        bayesglm(posterior_blue_I ~ I(X_F - 2.5), 
                 data = df_initial_conf_source_blockpart,
                 family = "binomial"))
      
      df_beta_over_time_pooled$slope[i] <- m_initial_conf_by_news_station$coefficients[2,1]
      df_beta_over_time_pooled$slope_sem[i] <- m_initial_conf_by_news_station$coefficients[2,2]
      df_beta_over_time_pooled$intercept[i] <- m_initial_conf_by_news_station$coefficients[1,1]
      df_beta_over_time_pooled$intercept_sem[i] <- m_initial_conf_by_news_station$coefficients[1,2]
    }
  }
  
  return(df_beta_over_time_pooled)
}
