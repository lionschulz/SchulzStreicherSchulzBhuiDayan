# File that loads all the necessary files to run the agents and simulate games


agent_function_files <- c(
  
  # Simulate game ---
  "game_functions.R",
  
  # File with functions that wraps all the other functions to run the complete agent:
  "agent_run_functions.R",

  "run_learner_on_game.R",
  "get_station_only_trial_predictions.R",
    
  # Functions that run the updating of the belief state of the trustworthiness of
  # a source for both the WITH and WITHOUT feedback learner
  "agent_learner_functions.R",
  
  # Function that updates the belief state about for the WITHOUT feedback learner
  "agent_updating_function.R",
  
  # Functions that runs the within trial confidence updates
  # Also contains function that runs on station only trial
  "agent_within_trial_belief_update.R",
  
  # Helper function
  "agent_helper_functions.R"
  )

agent_function_files <- paste0("models/R/", agent_function_files)

for(agent_function_file in agent_function_files) {
  
  source(here(agent_function_file))
  
}