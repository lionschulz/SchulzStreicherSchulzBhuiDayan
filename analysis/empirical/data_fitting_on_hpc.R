library(here)
source(here("functions/load_all_functions_and_libraries.R"))

seed <- 1
num_of_CPU_cores_to_use <- 3
num_iterations <- 200

print("Setting up data fitting function")

fit_on_hpc_main <- function(
    seed = 1,
    num_of_CPU_cores_to_use = 2,
    num_iterations = 5,
    
    #### Define which models to fit:
    vector_of_limited_models = c(FALSE, "one_prior", "one_prior_and_forgetting"),
    vector_of_conf_transform_options = c(FALSE, TRUE),
    vector_of_learn_one_likelihood = c(FALSE),
    
    # Legacy compatible with original data
    legacy_data = TRUE,
    data_folder = "",
    
    # Impose a setting for trying out new code
    only_use_first_x_participants = NULL
    ){
  
  
  ## Loading files and functions

  #### Get data ----
  
  print("Loading data")
  
  if(legacy_data){
    print("Loading Legacy Data")
    
    folders_new_data <- c(
      here("data/220118_mturk"),
      here("data/220119_mturk"),
      here("data/220131_mturk"),
      here("data/220206_mturk"),
      here("data/220207_mturk")
    )
    
    folders_old_data <- c(
      # here("data/210711_internal_pilot"),
      # here("data/210819_first_mturk_pilot"), # old version with 20 trials 
      here("data/210824_second_mturk_pilot"), 
      here("data/210826_third_mturk_pilot")
    )
    
    run_full_loading_exclusion_and_cleaning_pipeline_old_pilot(
      folders_new_data = folders_new_data,
      folders_old_data = folders_old_data,
      catch_question_limit = 4)
  } else {
    
    run_full_loading_exclusion_and_cleaning_pipeline_2023(
      data_folder,
      catch_question_limit = 2) # returns the dfs to the environment
  
  }
  

  print("Data loaded")
  
  
  #### Get data into format for fitting ----
  
  print("Processing data for fitting")
  print("Available completion codes (and their length):")
  all_completion_codes <- unique(df_participant_summary$completion_code)
  print(length(all_completion_codes))
  if(!is.null(only_use_first_x_participants)){
    df_main_task %<>% 
      filter(completion_code %in% all_completion_codes[1:only_use_first_x_participants])
  }
  
  data_for_fitting <- get_data_into_format_for_fitting(df_main_task)
  # if(!is.null(only_use_first_x_participants)){
  #   data_for_fitting <- data_for_fitting[1:only_use_first_x_participants]
  # }
  
  
  print("Data processed for fitting")
  
  
  #### Get parameter attributes and bounds
  
  print("Prep fitting settings and functions")
  parameter_ranges <- get_standard_parameter_ranges(
    prior_range = c(0.8, 80),
    weight_range = c(0.2, 3)
    )
  
  
  #### Get the list of functions used
  vector_of_functions_used <- get_vector_of_functions_used_for_fitting()
  
  print("Fitting prep done")
  
  

  
  print("Start fitting the following models:")
  print(
    expand.grid(
      vector_of_limited_models, 
      vector_of_conf_transform_options, 
      vector_of_learn_one_likelihood
    ) %>% 
      filter(
        ! (Var1 == "FALSE" & Var3 == TRUE)
      )
    )
  
  
  print("Start fitting loop")
  
  for (limited_model_to_fit in vector_of_limited_models) {
    
    for(conf_transform_to_fit in vector_of_conf_transform_options){
      
      for (current_learn_one_likelihood in vector_of_learn_one_likelihood) {
        
        if(current_learn_one_likelihood == TRUE & limited_model_to_fit != "one_prior"){
          
          print("Can't fit model with two priors and only learning one likelihood")
          
        } else {
      
          list_of_fits <- run_fitting_on_data_fitting_list(
            data_for_fitting = data_for_fitting, 
            fitting_real_data = TRUE, 
            df_participant_summary = df_participant_summary, 
            limited_model = limited_model_to_fit, 
            conf_transform = conf_transform_to_fit, 
            learn_one_likelihood = current_learn_one_likelihood,
            parameter_ranges = parameter_ranges, 
            vector_of_functions_used = vector_of_functions_used, 
            seed = seed,
            num_of_CPU_cores_to_use = num_of_CPU_cores_to_use,
            num_iterations = num_iterations
          )
        }
      }
    }
  }
  print("Full fitting pipeline finished")
}

print("Data fitting function now useable")
cat('fit_on_hpc_main(
    seed = 1,
    num_of_CPU_cores_to_use = 2,
    num_iterations = 5,
    
    #### Define which models to fit:
    vector_of_limited_models = c(FALSE, "one_prior", "one_prior_and_forgetting"),
    vector_of_conf_transform_options = c(FALSE, TRUE),
    
    # Impose a setting for trying out new code
    only_use_first_x_participants = NULL
  )')

