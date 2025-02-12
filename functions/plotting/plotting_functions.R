# Takes a number of plots provided through a list and stitches them together on
# one canvas using patchwork
combine_several_plots_from_list <- function(
    list_of_plots,
    num_cols = NULL,
    num_rows = NULL,
    plot_title = NULL,
    guides = NULL){
  
  pls <- list_of_plots[[1]]
  
  for (i in 2:length(list_of_plots)) {
    
    pls <- pls + list_of_plots[[i]]
    
  }
  
  pls <- pls + 
    plot_annotation(title = plot_title) +
    plot_layout(ncol = num_cols, nrow = num_rows, guides = guides)
  
  return(pls)
  
}


plot_source_attribute_overview <- function(game_attributes, without = FALSE){
  
  b_I_and_g_I_for_df <- c("blue", "green")
  possible_sources <- c("initial", 
                        "helpful",
                        "random",
                        "opposite", 
                        "greenbias",
                        "bluebias")
  
  df_plot_sources <- expand.grid(b_I_and_g_I_for_df, possible_sources)
  colnames(df_plot_sources) <- c("Colour", "Source")
  
  
  df_plot_sources$Probability <- 2
  df_plot_sources[df_plot_sources$Colour == "blue" && 
                    df_plot_sources$Source == "initial", ]$Probability <- game_attributes$b_I
  df_plot_sources[df_plot_sources$Colour == "green" & 
                    df_plot_sources$Source == "initial", ]$Probability <- game_attributes$g_I
  df_plot_sources[df_plot_sources$Colour == "blue" & 
                    df_plot_sources$Source == "helpful", ]$Probability <- game_attributes$b_F_helpful
  df_plot_sources[df_plot_sources$Colour == "green" & 
                    df_plot_sources$Source == "helpful", ]$Probability <- game_attributes$g_F_helpful
  df_plot_sources[df_plot_sources$Colour == "blue" & 
                    df_plot_sources$Source == "random", ]$Probability <- game_attributes$b_F_random 
  df_plot_sources[df_plot_sources$Colour == "green" & 
                    df_plot_sources$Source == "random", ]$Probability <- game_attributes$g_F_random
  df_plot_sources[df_plot_sources$Colour == "blue" & 
                    df_plot_sources$Source == "opposite", ]$Probability <- game_attributes$b_F_opposite
  df_plot_sources[df_plot_sources$Colour == "green" & 
                    df_plot_sources$Source == "opposite", ]$Probability <- game_attributes$g_F_opposite
  df_plot_sources[df_plot_sources$Colour == "blue" &
                    df_plot_sources$Source == "greenbias", ]$Probability <- game_attributes$b_F_greenbias
  df_plot_sources[df_plot_sources$Colour == "green" &
                     df_plot_sources$Source == "greenbias", ]$Probability <- game_attributes$g_F_greenbias
  df_plot_sources[df_plot_sources$Colour == "blue" & 
                    df_plot_sources$Source == "bluebias", ]$Probability <- game_attributes$b_F_bluebias
  df_plot_sources[df_plot_sources$Colour == "green" & 
                    df_plot_sources$Source == "bluebias", ]$Probability <- game_attributes$g_F_bluebias
  
  pl_source_attributes <- df_plot_sources %>% 
    filter(Source != without) %>% 
    mutate(Source = fct_rev(Source)) %>% 
    ggplot(aes(x = Source, y = Probability, fill = Colour)) + 
    geom_bar(stat = "identity",
             position = "dodge",
             alpha = 0.8) +
    scale_fill_manual(values = c(custom_colours$blue_probability, 
                                 custom_colours$green_probability),
                      name = "",
                      labels = c("p(blue ball | state = blue)", 
                                 "p(green ball | state = green)")) +
    scale_y_continuous(limits = c(0,1)) +
    geom_hline(yintercept = .5, alpha = .5, linetype = "dashed") +
    coord_flip()
  
  return(pl_source_attributes)
}



plot_source_distribution <- function(source_to_plot, game_attributes, grey = FALSE){
# Function that plots the likelihoods/distribution of each individual news station
  
  binom_probability <- list(blue = game_attributes[[paste0("b_F_", source_to_plot)]],
                            green = 1 - game_attributes[[paste0("g_F_", source_to_plot)]])
  binom_probability
  
  
  support <- 0:game_attributes$n_draws
  densities <- list(blue = dbinom(support, game_attributes$n_draws, binom_probability$blue),
                    green = dbinom(support, game_attributes$n_draws, binom_probability$green))
  if(grey){
    densities <- list(blue = rep(1,game_attributes$n_draws + 1)/game_attributes$n_draws,
                      green = rep(1,game_attributes$n_draws + 1)/game_attributes$n_draws)
  }
  
  df_densities <- data.frame(n_blue = rep(support, 2),
                             state = c(rep("green",6), rep("blue",6)),
                             density = c(densities$green,
                                         densities$blue))
  if(grey){
    colourscheme <- rep("grey",2)
  } else{
    colourscheme <- c(custom_colours$green_probability, custom_colours$blue_probability)
  }
  
  # print(colourscheme)
  
  plot <- df_densities %>% 
    mutate(state = fct_rev(state)) %>% 
    ggplot(aes(n_blue, density)) +
    geom_col(aes(fill = state), position = "dodge") + # custom_colours[[paste0(colour_to_plot,"_probability")]]) +
    scale_x_continuous(name = "Blue in news station", breaks = support) +
    scale_fill_manual(values = colourscheme) +
    facet_wrap(.~state)
  
  if(grey){
    plot <- plot + scale_y_continuous(limits = c(0,2*max(df_densities$density)))
  }
  
  return(plot)
}