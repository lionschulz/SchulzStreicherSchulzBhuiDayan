# Load functions that are part of the fitting pipeline

fitting_function_files <- c(
  
  "fitting_helper.R",
  
  "fitting_main_functions.R"
  
)


fitting_function_files <- paste0("functions/fitting/", fitting_function_files)

for(fitting_function_file in fitting_function_files) {
  
  source(here(fitting_function_file))
  
}