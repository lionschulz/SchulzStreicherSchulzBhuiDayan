run_fitting_on_data_fitting_list <- function(
    
  # data
  data_for_fitting,
  fitting_real_data = TRUE,
  df_participant_summary= NULL, # only if fitting real data
  
  # Model info
  limited_model = FALSE,
  conf_transform = FALSE,
  learn_one_likelihood = FALSE,
  parameter_ranges,
  parameters_not_to_fit_list = list(
    ground_truth = NA,
    bayes_learner = NA
  ),
  
  # Run-time info and cluster helper
  vector_of_functions_used,
  seed = 321,
  num_of_CPU_cores_to_use = 1,
  num_iterations = 200,
  
  identifier_of_generated_data = ""
  
){
  
  set.seed(seed)
  
  list_of_fits <- list()
  list_of_timings <- list()
  
  # Get participants -----------------------------------------------------------
  if(is.null(names(data_for_fitting))){ 
    # if unnamed participants (e.g. param recov)
    participants_to_fit <- 1:length(data_for_fitting)
  } else {
    participants_to_fit <- names(data_for_fitting)
  }
  
  print(participants_to_fit)
  
  if(identifier_of_generated_data != ""){
    print(" ------------ SIMULATED DATA INFO ------------ ")
    print(identifier_of_generated_data)
  }
  
  # Create basic message per model fit
  name_of_model_for_filename <- ifelse(
    limited_model == FALSE,
    "regular",
    limited_model
  )
  conf_transform_print <- ifelse(
    conf_transform,
    " with transformed confidence.",
    " without transformed confidence"
  )
  one_likelihood_print <- ifelse(
    learn_one_likelihood,
    " learning only one likelihood",
    " learning both likelihoods"
  )
  
  
  # Run every participant ------------------------------------------------------

  cl <- makeCluster(spec = num_of_CPU_cores_to_use)  
  clusterEvalQ(cl,library("tidyverse"))
  
  print("Cluster set up")
  
  for(i in 1:length(participants_to_fit)){
    
    print(" ------------ SYS INFO ------------ ")
    print(paste0("Number of cores: ", num_of_CPU_cores_to_use))
    print(paste0("Using seed: ", seed))
    print(paste0("Number of iterations seed: ", num_iterations))
    print("")
    
    print(" ------------ FIT INFO ------------ ")
    print(paste0("Model fit: ", name_of_model_for_filename, conf_transform_print, one_likelihood_print))
    print(paste(i, "out of", length(participants_to_fit)))
    print("")
    
    # Get participant and print info
    print(" ------------ PARTICIPANT INFO ------------ ") 
    current_participant <- participants_to_fit[i]
    
    if(fitting_real_data){
      df_participant_summary_subset <- df_participant_summary %>% 
        filter(completion_code == current_participant) %>% 
        select(completion_code, date, pilot_type, condition, max_block_number, num_of_main_trials)
      print(as.list(df_participant_summary_subset))
    }
    
    
    # We get the participant data and attributes
    # Data 
    if(fitting_real_data){
      # this might need to be refactored
      df_to_fit <- data_for_fitting[[current_participant]]$df_main_trials_all_sources
    } else {
      df_to_fit <- data_for_fitting[[current_participant]]$df_played_game
    }

    station_only_trials_to_fit <- data_for_fitting[[current_participant]]$station_only_trials
    current_sources_to_run <- names(station_only_trials_to_fit)
    
    # print
    print(current_sources_to_run)
    
    
    # Attributes
    print(" ------------ LEARNER TYPE ------------ ")
    current_learner_type <- data.frame(data_for_fitting[[current_participant]]$learner_type)
    print(as.list(current_learner_type))
    
    # Game attributes (e.g. num of trials, base probability)
    current_game_attributes <- data_for_fitting[[current_participant]]$game_attributes
    
    # Parameter Attributes
    print(current_learner_type$type)
    
    parameters_not_to_fit <- parameters_not_to_fit_list[[current_learner_type$type]]
    if(conf_transform){
      parameter_ranges$conf_transform_range <- parameter_ranges$conf_transform_range
    } else{
      parameter_ranges$conf_transform_range <- 0 # disables fitting of conf_transform
    }
    parameter_attributes <-  prep_parameters_for_fitting(
      feedback_condition = current_learner_type$type,
      prior_range = parameter_ranges$prior_range,
      weight_range = parameter_ranges$weight_range,
      noise_range = parameter_ranges$noise_range,
      forget_towards_range = parameter_ranges$forget_towards_range,
      conf_transform_range = parameter_ranges$conf_transform_range,
      how_to_fit_beta = "mean_uncertainty",
      parameters_not_to_fit = parameters_not_to_fit,
      limited_model = limited_model
    )
    
    ## Set-up cluster ----------------------------------------------------------
    
    variables_and_functions_used_for_parallelized_DEoptim <- c(
      vector_of_functions_used, 
      "get_neg_log_lik_several_sources",
      "current_sources_to_run",
      "df_to_fit",
      "parameter_attributes",
      "current_learner_type",
      "current_game_attributes",
      "station_only_trials_to_fit",
      "limited_model",
      "conf_transform"
    )
    

    # Export data to cluster
    clusterExport(
      cl = cl, 
      varlist = c(variables_and_functions_used_for_parallelized_DEoptim),
      envir = environment() 
      # environment defines the environment within the function
      # which we need because clusterExport would otherwise look at global vars
    )
    
    print("Exported data to cluster")

    print(" ------------ PARAMETER INFO ------------ ")
    print_parameters_to_fit_and_their_bounds(parameter_attributes)
    
    ptm <- proc.time() # this measures the system performance by timing
    
    
    # Do the actual fitting ----------------------------------------------------
    
    list_of_fits[[current_participant]] <- 
      DEoptim(
        function(x){
      
          # negative log-likelihood that is minimized
          get_neg_log_lik_several_sources(
            x,
            df_full_game =  df_to_fit,
            parameters_to_fit = parameter_attributes$parameters_to_fit,
            game_attributes = current_game_attributes,
            fixed_learner_attributes_df = current_learner_type,
            sources_to_run = current_sources_to_run,
            station_only_trials = station_only_trials_to_fit,
            limited_model = limited_model,
            conf_transform = conf_transform,
            learn_one_likelihood = learn_one_likelihood)
        },
        lower = parameter_attributes$bounds$lower, 
        upper = parameter_attributes$bounds$upper,
        control = DEoptim.control(
          cluster = cl,
          itermax = num_iterations
          )
      )
    
    list_of_timings[[current_participant]] <- proc.time() - ptm
    print(paste(i, "out of", length(participants_to_fit), "done"))
    print(list_of_timings[[current_participant]])

  }
  
  stopCluster(cl)
  
  timestamp_for_saving <- str_replace_all(Sys.time(), ":", "-")
  
  conf_transform_for_filename <- ifelse(conf_transform, "w_", "wo_")
  learn_one_likelihood_for_filename <- ifelse(learn_one_likelihood, "_onelik", "_twolik")
  
  name_of_file <- paste0(
    "_fits_", 
    name_of_model_for_filename, "_",
    conf_transform_for_filename, "ctrans",
    learn_one_likelihood_for_filename
  )
  
  if(!fitting_real_data){
    name_of_file <- paste0(name_of_file, "_from_", identifier_of_generated_data)
  }
  
  name_of_file <- paste0(name_of_file, ".rds")
  
  print(name_of_file)
  
  filepath <- ifelse(
    fitting_real_data,
    "analysis/empirical/Output/",
    "analysis/simulations/Output/"
    )
  
  saveRDS(
    list_of_fits,
    here(paste0(filepath, timestamp_for_saving, name_of_file))
  )
  
  return(list_of_fits)
  
}
