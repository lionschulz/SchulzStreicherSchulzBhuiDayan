### Data loading and basic cleaning ----

run_full_loading_exclusion_and_cleaning_pipeline_2023 <- function(
    data_location,
    catch_question_limit){
  # This returns to the outside environment and a processed and cleaned (as
  # well as excluded):
  #  - df_main_task 
  #  - df_participant_summary
  #  - df_main_task_wide
  
  data_location <- here(data_location)  
  
  list_of_files <- list()
  
  for (current_folder_name in data_location){
    list_of_files <- c(list_of_files,
                       dir(current_folder_name, full.names = TRUE, pattern = 'csv'))
  }
  
  # Load dataframes from the directory
  all_data_files <- lapply(list_of_files, read.csv)
  
  # Bind them all together into one dataframe
  df_complete <- bind_rows(all_data_files)
  df_complete$source_type <- get_sources_in_correct_order(df_complete$source_type)
  
  # doing some stuff to have the previous preprocessing pipeline work
  df_complete$date <- str_sub(df_complete$start_ISO_time, 1, 10)
  df_complete$pilot_type <- 'full_run'
  df_complete$hit_id <- "prolific_full"
  hit_codes <- c("prolific_full")
  
  # Get the data into the right format
  df_main_task <- get_df_main_task(df_complete, hit_codes)
  df_main_task_wide <- convert_data_df_into_simulation_like_df(df_main_task)
  list_of_summary_and_questionnaire_dfs <- get_df_participant_summary(
    df_complete, 
    df_main_task,
    with_questionnaires = TRUE)
  df_participant_summary <- list_of_summary_and_questionnaire_dfs$df_participant_summary
  
  # Run exclusion
  all_participants_to_exclude <- get_list_of_participants_to_exclude(
    df_main_task,
    df_participant_summary,
    catch_question_limit = catch_question_limit,
    old_or_new_data = "new",
    df_epist_trust_raw = list_of_summary_and_questionnaire_dfs$questionnaire_dfs_list$df_epist_raw
  )
  
  # Apply the exclusion
  df_participant_summary %<>%
    mutate(exclude = completion_code %in% all_participants_to_exclude
    )
  df_participant_summary %<>%
    filter(exclude == FALSE)
  df_complete %<>%
    filter(! completion_code %in% all_participants_to_exclude)
  df_main_task %<>%
    filter(! completion_code %in% all_participants_to_exclude)
  df_main_task_wide %<>%
    filter(! completion_code %in% all_participants_to_exclude)

  print("N participants included:")
  print(length(unique(df_complete$completion_code)))
  print(length(unique(df_main_task$completion_code)))
  print(length(unique(df_main_task_wide$completion_code)))
  
  # Return objects to environment for further processing
  df_complete <<- df_complete
  df_main_task <<- df_main_task
  df_participant_summary <<- df_participant_summary
  df_main_task_wide <<- convert_data_df_into_simulation_like_df(df_main_task)
}



run_full_loading_exclusion_and_cleaning_pipeline_old_pilot <- function(
    folders_new_data,
    folders_old_data,
    catch_question_limit){
  # !!! For 2022 pilots !!!
  # This returns to the outside environment and a processed and cleaned (as
  # well as excluded):
  #  - df_main_task 
  #  - df_participant_summary
  #  - df_main_task_wide
  
  # Load data
  df_complete <- load_and_process_all_data(folders_new_data, folders_old_data)
  
  completion_codes <- unique(df_complete$completion_code)
  number_of_participants <- length(completion_codes)
  
  hit_codes <- unique(df_complete$hit_id)
  cat(
    paste0(
      "Number of total participants: ", 
      number_of_participants, 
      "\nN. hits: ", length(hit_codes))
  )
  
  # Slightly process data
  df_main_task <- get_df_main_task(df_complete, hit_codes)
  df_participant_summary <- get_df_participant_summary(df_complete, df_main_task)
  
  # Run exclusions
  df_main_task_before_exclusion <- df_main_task
  df_participant_summary_before_exclusion <- df_participant_summary
  
  participants_to_exclude <- get_list_of_participants_to_exclude(
    df_main_task_before_exclusion,
    df_participant_summary_before_exclusion,
    catch_question_limit = catch_question_limit
  )
  
  # Actually exclude participants
  df_complete %<>% 
    filter(! completion_code %in% participants_to_exclude$full)
  
  df_main_task %<>% 
    filter(! completion_code %in% participants_to_exclude$full) %>% 
    mutate(side_chosen = sign(response -50)) # green: 0, blue: 1
  
  df_participant_summary %<>%
    filter(! completion_code %in% participants_to_exclude$full)
  
  
  # Return the objects to the full environment
  df_complete <<- df_complete
  df_main_task <<- df_main_task
  df_participant_summary <<- df_participant_summary
  df_main_task_wide <<- convert_data_df_into_simulation_like_df(df_main_task)
}



# combine files from a list of folders into one dataframe
get_df_complete <- function(folder_names){
  list_of_files <- list()
  
  for (current_folder_name in folder_names){
    
    list_of_files <- c(list_of_files,
                       dir(current_folder_name, full.names = TRUE, pattern = 'csv'))
  }
  
  # Load dataframes from the directory
  all_data_files <- lapply(list_of_files, read.csv)
  
  # Bind them all together into one dataframe
  df_complete <- bind_rows(all_data_files)
  
  return(df_complete)
}


# Get both old and new data into the same format and one dataframe
# and do some preprocessing
load_and_process_all_data <- function(folder_names_new_data, folder_names_old_data){
  
  # Load the data
  df_new_data <- get_df_complete(folder_names_new_data)
  df_new_data$pilot_type <- "2022_Backward/Forward"
  
  df_old_data <- get_df_complete(folder_names_old_data)
  df_old_data$pilot_type <- "2021"
  
  # Reformat old data
  df_old_data_reformatted <- df_old_data %>% 
    mutate(news_station_first = "Second") %>%  
    rename(num_blue_indep = num_blue_i,
           num_blue_news = num_blue_f,
           colours_indep = colours_i,
           colours_news = colours_f)
    # since all of the old pilots have news station second

  # Combine
  df_complete <- bind_rows(
    df_new_data,
    df_old_data_reformatted
  )
  
  # Formatting and ordering
  df_complete$source_type <- get_sources_in_correct_order(df_complete$source_type)
  
  df_complete$plugin_type <- 
    factor(df_complete$plugin_type,      
           levels = c("initial_source",
                      "full_source"))
  
  df_complete$date <- str_sub(df_complete$start_ISO_time, 1, 10)
  
  return(df_complete)
}


# Orders sources into regular order
get_sources_in_correct_order <- function(source_type_variable){
  
  return(
    factor(source_type_variable,
           levels = c("helpful", "random", "opposite", "bluebias", "greenbias"))
  )
  
}


# Get data down to main task trials and do some preprocessing
get_df_main_task <- function(df_complete, hit_codes){
  
  # Basic variable selection and trial filtering
  df_main_task <- df_complete %>% 
    select(-success, -timeout, -failed_images, -failed_audio, -failed_video,
           -trial_type, -trial_index, -time_elapsed, -internal_node_id, 
           -start_ISO_time, -filename_for_csv, -times_comprehension_check_wrong,
           -view_history, -question_order, -question, -game_attributes) %>% 
    filter(experiment_stage == "main" &
             plugin_type %in% c("initial_source", "full_source") &
             source_type != "practice_source"
    ) %>% # exclude practice trials
    mutate(response = as.numeric(response),
           accuracy = as.numeric(accuracy),
           response_absolute = (abs(response - 50) + 50),
           block_half = NA,
           block_quarter = NA)
  
  
  # mark half and quarter points
  for(current_hit_id in hit_codes) {
    
    num_of_trials <- max(df_main_task %>% 
                           filter(block_stage != "station_only",
                                  hit_id == current_hit_id) %>% 
                           select(trial_number_within_block))
    
    block_half_breaks <- c(0,round(num_of_trials/2),num_of_trials)
    df_main_task[df_main_task$hit_id == current_hit_id,]$block_half <-
      .bincode(df_main_task[df_main_task$hit_id == current_hit_id,]$trial_number_within_block, 
               block_half_breaks) 
    
    block_quarter_breaks <- c(0,
                              round(num_of_trials/4),
                              round(num_of_trials/2),
                              round(3*num_of_trials/4),
                              num_of_trials)
    
    df_main_task[df_main_task$hit_id == current_hit_id,]$block_quarter <-
      .bincode(df_main_task[df_main_task$hit_id == current_hit_id,]$trial_number_within_block,
               block_quarter_breaks)
  }
  
  return(df_main_task)
}

# Makes a summary df with one participant per row and information 
# about task performance and demographics
get_df_participant_summary <- function(
    df_complete, 
    df_main_task,
    with_education = FALSE,
    with_questionnaires = FALSE){
  
  # Basic info
  df_participant_summary <- df_complete %>% 
    group_by(completion_code, worker_id, hit_id, start_ISO_time, date, pilot_type) %>% 
    summarise(
      times_comprehension_wrong = max(times_comprehension_check_wrong, na.rm = TRUE),
      score = max(total_quadratic_score, na.rm = TRUE),
      condition = max(condition),
      max_block_number = max(block_number, na.rm = TRUE),
      news_station_first = max(news_station_first),
      time_on_task_in_ms = max(time_elapsed),
      time_on_task = convert_ms_to_min_sec_string(max(time_elapsed))
    )
  
  # Add timing of main task
  df_participant_summary <- merge(
    df_participant_summary, 
    df_complete %>% 
      filter(experiment_stage == 'main') %>% 
      group_by(completion_code) %>% 
      summarise(
        time_on_main_task_in_ms = max(time_elapsed) - min(time_elapsed),
        time_on_main_task =
          convert_ms_to_min_sec_string(max(time_elapsed) - min(time_elapsed)))
  )
  
  
  ## Add demographics
  df_demographics <- df_complete %>%
    filter(trial_type == "survey-html-form") %>%  # this is the only one that uses this plugin. Changed in new version (17.04.23 to also included experiment stage marker)
    select(completion_code, response) %>% 
    mutate(age = NA, gender = NA, education = NA)
  
  for (row in 1:nrow(df_demographics)) {
    df_demographics[row, ]$age <- 
      as.integer(fromJSON(df_demographics[row, ]$response)$age)
    df_demographics[row, ]$gender <- 
      fromJSON(df_demographics[row, ]$response)$gender
    df_demographics[row, ]$education <- 
      fromJSON(df_demographics[row, ]$response)$education
  }
  
  df_demographics <- df_demographics %>% 
    select(-response)
  
  df_participant_summary <- merge(
    df_participant_summary,
    df_demographics
  )
  
  # df_participant_summary[df_participant_summary$age == 99,]$age <- 26
  
  # hit_with_fewer_main_trials <- c("36BTXXLZ3YCRDQSR6C13CH0L3AC4RU", "internal")
  
  # add number of trials
  df_participant_summary <- left_join(
    df_participant_summary,
    df_main_task %>% 
      group_by(completion_code) %>% 
      summarize(num_of_main_trials = max(trial_number_within_block) - 6),
    by = "completion_code"
  )
  
  df_participant_summary %>% 
    arrange(start_ISO_time)
  
  if(with_questionnaires){
    questionnaire_dfs_list <- process_questionnaire_data(df_complete)
    
    df_participant_summary <- left_join(
      df_participant_summary,
      questionnaire_dfs_list$df_questionnaires_wide,
      by = "completion_code"
    )
  }
  
  if(with_questionnaires){
    to_return <- list(
      df_participant_summary = df_participant_summary,
      questionnaire_dfs_list = questionnaire_dfs_list
    )
  } else {
    to_return <- df_participant_summary
  }

  return(to_return)
    
}



################################################################################


# Gets list of participants to exclude
get_list_of_participants_to_exclude <- function(
    df_main_task,
    df_participant_summary,
    catch_question_limit,
    old_or_new_data = 'old', # added to work with 2023 news station only run
    df_epist_trust_raw = NA){
  
  participants_to_exclude_based_on_comprehension_checks <- 
    unique(
      filter(df_participant_summary,
             times_comprehension_wrong > catch_question_limit)$completion_code
      )
   
  print(paste0("N Participants with more than ", catch_question_limit,
         " correct catch questions: ",
         length(participants_to_exclude_based_on_comprehension_checks)))
  
  print(paste0("Participant completion codes with more than ", catch_question_limit,
         " correct catch questions: "))
  print(participants_to_exclude_based_on_comprehension_checks)

  if(old_or_new_data == "old"){
    df_main_task <- df_main_task %>% 
      mutate(side_chosen = sign(response -50)) # green: 0, blue: 1
    
    df_off_target <- df_main_task %>% 
      filter(block_stage != "station_only",
             plugin_type == "initial_source",
             news_station_first == "Second") %>%
      group_by(completion_code, news_station_first, num_blue_indep) %>%
      summarise(p_blue = 0.5 * (1 + mean(side_chosen))) %>% 
      mutate(target_p = ifelse(num_blue_indep <= 2, 0, 1),
             off_target = abs(target_p - p_blue))
    # df_off_target
    
    # print(length(unique(df_off_target$completion_code)))
    
    df_off_target_error <- df_off_target %>% 
      group_by(completion_code) %>% 
      summarize(sum_error = sum(off_target))
    
    # print(df_off_target_error)
    # print(length(unique(df_off_target_error$completion_code)))
    
    
    df_off_target_large_error <- filter(df_off_target_error,
                                        sum_error > 1.5)
    
    
    participants_with_high_initial_random_behav <-
      filter(df_off_target_error, sum_error > 1.5)$completion_code
    
    # print(participants_with_high_initial_random_behav)
    
    participants_exclude_full <- 
      union(participants_with_high_initial_random_behav, 
            participants_to_exclude_based_on_comprehension_checks)
    
  } else {
    
    participants_to_exclude_based_on_catch_question <- unique(
      df_epist_trust_raw %>% 
        filter(
          subscale == 'catch',
          response != 3
        )
    )$completion_code
    
    print(paste0("N Participants with wrong catch question:",
           length(participants_to_exclude_based_on_catch_question)))
    
    print("Participant completion codes with wrong catch question: ") 
    print(participants_to_exclude_based_on_catch_question)
    
    participants_exclude_full <- union(
      participants_to_exclude_based_on_comprehension_checks,
      participants_to_exclude_based_on_catch_question)
    
  }
  
  print(paste0("N Participants to exclude:",
               length(participants_exclude_full)))
  
  print("Participant completion codes to exclude")
  print(participants_exclude_full)
  
  # exclusion_list <- list(
  #   full = participants_exclude_full,
  #   initial_random_behaviour = participants_with_high_initial_random_behav,
  #   catch_question_limit = participants_to_exclude_based_on_comprehension_checks
  # )
  
  return(participants_exclude_full)
}












# Get questionnaire responses
process_questionnaire_data <- function(df_complete){
  # Returns a list of dataframes with raw and summary statistics for the questionnaires
  
  # Epistemic trust ------------------------------------------------------------
  
  # Raw processing
  
  df_epist_trust <- df_complete %>% 
    filter(experiment_stage == 'EpistemicTrust')
  
  epistemic_trust_questionnaire_length <- 16
  
  df_epist_trust_processed <- data.frame(
    completion_code = rep(unique(df_epist_trust$completion_code),
                          each = epistemic_trust_questionnaire_length),
    question_index = seq(1, epistemic_trust_questionnaire_length),
    response = NA
  )
  
  epistemic_trust_questionnaire_responses <- c()
  
  for(i in 1:nrow(df_epist_trust)){
    epistemic_trust_questionnaire_responses <- c(
      epistemic_trust_questionnaire_responses,
      unlist(fromJSON(df_epist_trust[i,]$response[[1]]), use.names = FALSE)
    )
  }
  
  df_epist_trust_processed$response <- epistemic_trust_questionnaire_responses
  
  epist_trust_questionnaire_info <- list(
    # okay, turns out these are all poled in the same direction
    "1" = list(pol = 1, subscale = "trust"),
    "2" = list(pol = 1, subscale = "trust"),
    "3" = list(pol = 1, subscale = "mistrust"),
    "4" = list(pol = 1, subscale = "mistrust"),
    "5" = list(pol = 1, subscale = "credulity"),
    "6" = list(pol = 1, subscale = "credulity"),
    "7" = list(pol = 1, subscale = "trust"),
    "8" = list(pol = 1, subscale = "trust"),
    "9" = list(pol = 1, subscale = "mistrust"),
    "10" = list(pol = 1, subscale = "catch"),
    "11" = list(pol = 1, subscale = "mistrust"),
    "12" = list(pol = 1, subscale = "credulity"),
    "13" = list(pol = 1, subscale = "credulity"),
    "14" = list(pol = 1, subscale = "trust"),
    "15" = list(pol = 1, subscale = "mistrust"),
    "16" = list(pol = 1, subscale = "credulity")
  )
  
  epist_trust_subscale_vector <- c()
  for (i in 1:length(epist_trust_questionnaire_info)) {
    epist_trust_subscale_vector <- c(
      epist_trust_subscale_vector,
      epist_trust_questionnaire_info[[i]]$subscale
    )
  }
  df_epist_trust_processed$subscale <- epist_trust_subscale_vector
  
  # Summary stats
  df_epist_trust_summary <- df_epist_trust_processed %>% 
    group_by(completion_code, subscale) %>% 
    summarise(mean_response = mean(response)) %>% 
    pivot_wider(id_cols = 'completion_code', names_from = 'subscale', values_from = 'mean_response')
  

  # World Values Survey -------------------------------------------------------
  
  df_wvs <- df_complete %>% 
    filter(experiment_stage == "WorldValues") %>% 
    select(completion_code, response) %>% 
    rename(wvs = response) %>% 
    mutate(wvs = as.numeric(wvs))

  
  # News Trust -----------------------------------------------------------------
  
  df_news_trust_unprocessed <- df_complete %>% 
    filter(experiment_stage == "NewsTrust")
  
  for(i in 1:nrow(df_news_trust_unprocessed)){
    
    response_vector_one_participant <- unlist(fromJSON(df_news_trust_unprocessed$response[i]))
    
    df_news_trust_one_participant <- data.frame(
      completion_code = df_news_trust_unprocessed$completion_code[i],
      media = names(response_vector_one_participant),
      response = response_vector_one_participant
    )
    
    if(i == 1){
      df_news_trust <- df_news_trust_one_participant
    } else {
      df_news_trust <- rbind(df_news_trust, df_news_trust_one_participant)
    }
  }
  
  df_news_trust_wide <- df_news_trust %>% 
    pivot_wider(id_cols = "completion_code", 
                names_from = "media", 
                values_from = "response",
                names_prefix = "trust_")
  
  
  # News_Frequencies -----------------------------------------------------------
  df_news_freq_unprocessed <- df_complete %>% 
    filter(experiment_stage == "NewsFrequency") 
  df_news_freq_unprocessed$response[1]
  
  for(i in 1:nrow(df_news_freq_unprocessed)){
    
    df_news_freq_one_participant <- data.frame(
      completion_code = df_news_freq_unprocessed$completion_code[i],
      news_category = names(unlist(fromJSON(df_news_freq_unprocessed$response[i]))),
      rating = unlist(fromJSON(df_news_freq_unprocessed$response[i]), use.names = FALSE)
    )
    
    if(i == 1){
      df_news_freq <- df_news_freq_one_participant
    } else {
      df_news_freq <- rbind(df_news_freq,df_news_freq_one_participant)
    }
  }
  
  df_news_freq_wide <- df_news_freq %>% 
    pivot_wider(
      id_cols = 'completion_code',
      names_from = 'news_category', 
      values_from = 'rating',
      names_prefix = 'frequency_')
  
  
  # News Sources ---------------------------------------------------------------
  df_news_sources_unprocessed <- df_complete %>% 
    filter(experiment_stage == 'Sources') 
  
  possible_news_sources <- c(
    "ABC News",
    "Breitbart",
    "CBS News",
    "CNN",
    "Fox News",
    "MSNBC",
    "NBC News",
    "New York Times",
    "NPR",
    "Wall Street Journal",
    "Washington Post",
    "USA Today",
    "Other (enter on next page)"
  )
  
  possible_social_media_sources <- c(
    "Facebook",
    "Instagram",
    "Twitter",
    "WhatsApp",
    "Snapchat",
    "TikTok",
    "YouTube",
    "Reddit",
    "LinkedIn",
    "Other (enter on next page)"
  )
  
  for(i in 1:nrow(df_news_sources_unprocessed)){
    
    response_vector_one_participant <- fromJSON(df_news_sources_unprocessed$response[i])
    
    df_news_sources_one_participant <- data.frame(
      completion_code = df_news_sources_unprocessed$completion_code[i],
      media_type = c(
        rep('media', length(possible_news_sources)), 
        rep('social_media', length(possible_social_media_sources))
      ),
      media_source = c(possible_news_sources, possible_social_media_sources),
      response = c(
        as.numeric(possible_news_sources %in% response_vector_one_participant$Media),
        as.numeric(possible_social_media_sources %in% response_vector_one_participant$SocialMedia)
      )
    )
    
    if(i == 1){
      df_news_sources <- df_news_sources_one_participant
    } else {
      df_news_sources <- rbind(
        df_news_sources,
        df_news_sources_one_participant
      )
    }
    
  }
  df_news_sources_wide <- df_news_sources %>% 
    select(completion_code, media_source, response) %>% 
    filter(media_source != 'Other (enter on next page)') %>% 
    pivot_wider(id_cols = 'completion_code', names_from = 'media_source', values_from = 'response')
  
  
  # Trust and improvement of the sources 
  df_post_block_trust_improve <- df_complete %>% 
    filter(block_stage == 'questionnaires') %>% 
    select(completion_code, source_type, question, response) %>% 
    mutate(response = as.numeric(response))
  
  df_post_block_trust_improve_wide <- df_post_block_trust_improve %>% 
    pivot_wider(
      id_cols = 'completion_code',
      values_from = 'response',
      names_from = c('source_type', 'question')
    )
  
  df_questionnaires_wide <- Reduce(
    function(x, y) merge(x, y, all=TRUE, by = 'completion_code'),
    list(
      df_epist_trust_summary,
      df_wvs,
      df_news_trust_wide,
      df_news_freq_wide,
      df_news_sources_wide,
      df_post_block_trust_improve_wide
      )
    )
  
  # Output all
  return(
    list(
      df_questionnaires_wide = df_questionnaires_wide,
      df_epist_raw = df_epist_trust_processed,
      df_epist_summary = df_epist_trust_summary,
      df_wvs = df_wvs,
      df_news_trust = df_news_trust,
      df_news_freq = df_news_freq,
      df_news_sources = df_news_sources,
      df_post_block_trust_improve = df_post_block_trust_improve
    )
  )
  
}


################################################################################

### Process summary file ----

# function that translates the points gained on the task to the bonus
translate_points_to_bonus <- function(points, 
                                      base_points,
                                      base_bonus, 
                                      dollar_per_point, 
                                      max_bonus){
  bonus <- (points - base_points) * dollar_per_point + base_bonus
  
  # make sure that the bonus doesn't exceed the bounds
  bonus <- ifelse(bonus > max_bonus, max_bonus, bonus)
  bonus <- ifelse(bonus < 0, 0, bonus)
}


convert_ms_to_min_sec_string = function(time_elapsed){
  time_on_task <- max(time_elapsed)
  minutes_on_task = time_on_task / 1000 / 60
  seconds_on_task = minutes_on_task %% 1 * 60
  return(paste0(floor(minutes_on_task), ":", floor(seconds_on_task)))
}

convert_ms_to_min <- function(ms) {
  return(ms / (1000 * 60))
}
