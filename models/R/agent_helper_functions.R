# Functions for the odds

# Compute odds
odds_blue <- function(X, n_draws, p_blue){
  (p_blue / (1 - p_blue)) ^ (2*X - n_draws) 
}


# Inverse logit (sigmoid function)
inverse_logit <- function(x){
  1 / (1 + exp(-x))
}


# Normalizes element to sum to 1
sum_to_1 <- function(x){
  return(x/sum(x))
}

# Transform confidence non-linearily
transform_conf <- function(conf, k){
  return(conf^k/(conf^k + (1-conf)^k))
}



get_logodds_a_I_and_c_I <- function(X_vector,
                                    n_draws,
                                    probability_blue){
  # computes log-odds, action, and confidence based on a vector of random variables
  
  initial_decision_list <- list()
  
  initial_decision_list$logodds_blue <- log(odds_blue(X_vector,
                                                 n_draws,
                                                 probability_blue))
  
  initial_decision_list$a_i <- sign(initial_decision_list$logodds_blue)
  
  initial_decision_list$c_i <- inverse_logit(initial_decision_list$a_i *
                                          initial_decision_list$logodds_blue)
  
  # unsigned posterior of blue for the news station first case
  initial_decision_list$posterior_blue <- inverse_logit(initial_decision_list$logodds_blue)
  
  return(initial_decision_list)
}


# Function that selects the necessary balls for the learning and returns 
# them in a list
select_source_vectors <- function(df_game, source_to_play_with, n_draws){
  
  n_blue_for_learner_v <- df_game %>% 
    select(ends_with(source_to_play_with))

  n_balls_source <- list()
  n_balls_source$X_I <- df_game$X_I
  n_balls_source$n_blue_for_learner_v <- n_blue_for_learner_v[[1]]
  n_balls_source$n_green_for_learner_v <- n_draws - n_blue_for_learner_v[[1]]# port df -> vector
  
  
  return(n_balls_source)
}


# Function that takes the summary parameters from a beta-distribution and
# returns the actual beta-distribution parameters alpha and beta
get_beta_parameters_from_summary <- function(beta_mean,
                                             beta_uncertainty){
  
  a <- beta_uncertainty * beta_mean
  
  b <- beta_uncertainty - a
  
  return(list(a = a,b = b))
  
}


