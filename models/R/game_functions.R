
# Functions that simulate a game for the 


# Generic simulator of source based on a specific game setting,
# with given probabilities, and given a vector of ground truths
get_source <- function(game_settings_l, p_blue, p_green, ground_truth){
  
  
  # if unbiased p_green == p_blue
  X_when_blue <- rbinom(game_settings_l$n_trials, 
                        game_settings_l$n_draws, 
                        p_blue) # n_green when gt = blue
  
  X_when_green <- rbinom(game_settings_l$n_trials,
                          game_settings_l$n_draws, 
                          1 - p_green) # n_green when gt = green

  
  X_v <- vector(length = game_settings_l$n_trials)
  X_v[ground_truth == -1] <- X_when_green[ground_truth == -1]
  X_v[ground_truth == 1] <- X_when_blue[ground_truth == 1]
  
  return(X_v)
}


 
# Function that returns a dataframe of the urn game given the game_settings
# At the moment, this returns all possible sources for a given ground truth
get_game <- function(game_settings_l, source_type = "helpful"){
  
  # Example input list with all necessary arguments:
  # game_attributes <- list(n_trials = 100,     # number of trials
  #                         n_draws = 5,        # number of balls to be drawn
  #                         b_I = .7,           # INITIAL DRAW
  #                         g_I = .7,
  #                         b_F_helpful = .7,   # FINAL (SOURCE) DRAW: 
  #                         g_F_helpful = .7,
  #                         b_F_random = .5,
  #                         g_F_random = .5,
  #                         b_F_opposite = .3,
  #                         g_F_opposite = .3,
  #                         b_F_greenbias = .5,
  #                         g_F_greenbias = .7,
  #                         b_F_bluebias = .7,
  #                         g_F_bluebias = .5
  # )
  
  
  # picks the correct urn on that trial
  ground_truth <- sample(c(rep(0,game_settings_l$n_trials/2),
                           rep(1,game_settings_l$n_trials/2), 
                           rep(rbinom(1,1,.5),game_settings_l$n_trials %%2)))
  # last line fills in if game_settings_l$n_trials not even
  # 0 = green
  # 1 = blue
  ground_truth <- ground_truth * 2 - 1 # port to green = -1, blue = 1

  # initial source -- always helpful
  X_I <- get_source(game_settings_l,
                          game_settings_l$b_I,
                          game_settings_l$g_I,
                          ground_truth)
  
  
  b_F_string <- paste0("b_F_", source_type)
  g_F_string <- paste0("g_F_", source_type)
  b_F <- game_settings_l[[b_F_string]]
  g_F <- game_settings_l[[g_F_string]]
  
  X_F <- get_source(game_settings_l,
                    b_F,
                    g_F,
                    ground_truth)
  
  df_game <- data.frame(trial = 1:game_settings_l$n_trials,
                   ground_truth,
                   X_I,
                   X_F,
                   source_type,
                   news_station_first = game_settings_l$news_station_first)
  
  return(df_game)
}



## Function that gets game with several sources
get_game_with_several_sources <- function(game_settings,
                                          which_sources){
  
  # first source
  df_out <- get_game(game_settings,
                     which_sources[1])
  
  if (length(which_sources) > 1) {
    for (i in 2:length(which_sources)) {
      df_out <- rbind(df_out,
                      get_game(game_settings,
                               which_sources[i]))
    }
  }
  
  return(df_out)
}