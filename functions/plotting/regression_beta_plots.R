plot_inidividual_betas_and_hist <- function(
    df_regression_betas,
    news_station_first_chosen, 
    chosen_beta,
    sort_by_source_beta = FALSE){
  # Produces a plot with two parts: The individual betas and their sd on top
  # (possibly sorted by sort_by_beta) and the distribution of these values below
  
  
  errorbar_base <- c(chosen_beta, paste0(chosen_beta, "_sd"))
  errorbar_min <- paste0(errorbar_base[1], " - ", errorbar_base[2])
  errorbar_max <- paste0(errorbar_base[1], " + ", errorbar_base[2])
  
  
  df_regression_betas_subset <- df_regression_betas %>% 
    filter(news_station_first %in% news_station_first_chosen)
  
  if(sort_by_source_beta != FALSE){
    df_for_order <- df_regression_betas_subset %>% 
      filter(source_type == sort_by_source_beta) %>% 
      arrange(get(chosen_beta))
    
    df_regression_betas_subset$completion_code <-
      fct_relevel(df_regression_betas_subset$completion_code,
                  rev(unique(df_for_order$completion_code)))
  }
  
  
  axis_lim_high <- 1.02 * max(df_regression_betas_subset[[chosen_beta]] + df_regression_betas_subset[[errorbar_base[2]]],
                              na.rm = TRUE) 
  axis_lim_low <- 1.02 * min(df_regression_betas_subset[[chosen_beta]] - df_regression_betas_subset[[errorbar_base[2]]],
                             na.rm = TRUE) 
  
  max_axis_lim <- max(abs(c(axis_lim_high, axis_lim_low)))
  
  bin_size <- (max_axis_lim - (-1) * max_axis_lim) / 50
  
  if(length(news_station_first_chosen) > 1){
    news_station_first_chosen <- "First and Second"
  }
  
  title_string <- paste0(
    "News Station ", news_station_first_chosen, "\n",
    chosen_beta
  )
  
  
  pl_individual_betas <- df_regression_betas_subset %>% 
    ggplot(aes_string("completion_code", chosen_beta, colour = "source_type")) +
    geom_point() +
    scale_y_continuous(limits =  c(axis_lim_low, axis_lim_high)) +
    geom_errorbar(aes_string(ymin = errorbar_min,
                             ymax = errorbar_max),
                  width = .2) +
    scale_color_manual(values = custom_colours$source_types,
                       name = "News\nStation") +
    geom_hline(yintercept =  0) +
    facet_grid(.~source_type) +
    theme(axis.text.y= element_blank(),
          axis.title.x = element_blank()) +
    coord_flip()+
    ggtitle(title_string) +
    guides(color = "none")
  pl_individual_betas
  
  pl_betas_hist <- df_regression_betas_subset %>% 
    ggplot(aes_string(x = chosen_beta, fill = "source_type", colour = "source_type")) +
    geom_histogram(binwidth = bin_size, position = "dodge", alpha = .3,
                   aes(y = ..density..)) +
    scale_color_manual(values = custom_colours$source_types,
                       name = "News\nStation") +
    scale_fill_manual(values = custom_colours$source_types,
                      name = "News\nStation") +
    geom_vline(xintercept =  0) +
    coord_cartesian(xlim = c(axis_lim_low, axis_lim_high)) +
    facet_grid(.~source_type) + 
    theme(strip.text = element_blank()) +
    guides(colour = "none", fill = "none")
  pl_betas_hist
  
  pl_full <- pl_individual_betas / pl_betas_hist + plot_layout(heights = c(1.5,1))
  
  return(pl_full)
}


################################################################################


plot_two_different_slopes_for_same_source_and_news_station <- function(
    df_regression_betas,
    slopes_to_plot,
    source_to_plot,
    news_station_first_to_plot,
    coord_cartesian_x = NULL,
    coord_cartesian_y = NULL,
    plot_abline = FALSE,
    aspect_ratio_scatter = 1,
    plot_to_return = "all",
    point_size = 3
){
  
  num_bins <- 30
  
  sources_to_investigate = c("helpful", "random", "opposite", "bluebias")
  colour_plot <- 
    custom_colours$source_types[which(sources_to_investigate == source_to_plot)]
  
  alpha_level_for_0_point = .3
  
  df_subset <- df_regression_betas %>% 
    filter(source_type == source_to_plot,
           news_station_first %in% news_station_first_to_plot)
  
  if(length(news_station_first_to_plot) > 1) {
    news_station_first_to_plot <- "first and second"
  }
  
  plot_title <- paste0(
    toTitleCase(source_to_plot), " source: (News Station ",
    news_station_first_to_plot, ")\n",
    slopes_to_plot[1], " and ", slopes_to_plot[2]
  )
  
  plot_scatter <- df_subset %>% 
    ggplot(aes_string(x = slopes_to_plot[1], slopes_to_plot[2])) +
    geom_hline(yintercept = 0, alpha = alpha_level_for_0_point,
               linetype = "dashed") +
    geom_vline(xintercept = 0, alpha = alpha_level_for_0_point,
               linetype = "dashed") +
    geom_smooth(method='lm', alpha = .2, colour = "grey") +  
    geom_point(colour = colour_plot,
               size = point_size, 
               alpha = .8) +
    scale_color_manual(values=met.brewer("Peru1", 2)) +
    stat_cor() +
    theme(aspect.ratio = aspect_ratio_scatter)
  
  if(plot_abline){
    plot_scatter <- plot_scatter +
      geom_abline(alpha = alpha_level_for_0_point,
                  linetype = "dashed")
  }
  
  plot_histogram_x <- df_subset %>% 
    ggplot(aes_string(slopes_to_plot[1])) +
    geom_histogram(
      colour = colour_plot,
      fill = colour_plot,
      bins = num_bins,
      position = "dodge", 
      alpha = .3,
      aes(y = ..density..)) +
    geom_vline(xintercept = 0, alpha = alpha_level_for_0_point) +
    scale_color_manual(values=met.brewer("Peru1", 2)) +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
  
  if(!is.null(coord_cartesian_x)){
    plot_histogram_x <- 
      plot_histogram_x + coord_cartesian(xlim = coord_cartesian_x)
    plot_scatter <- 
      plot_scatter + scale_x_continuous(limits = coord_cartesian_x)
  }
  
  plot_histogram_y <- df_subset %>% 
    ggplot(aes_string(slopes_to_plot[2])) +
    geom_histogram(
      colour = colour_plot,
      fill = colour_plot,
      bins = num_bins,
      position = "dodge", 
      alpha = .3,
      aes(y = ..density..)) +
    geom_vline(xintercept = 0, alpha = alpha_level_for_0_point) +
    scale_color_manual(values=met.brewer("Peru1", 2)) +
    coord_flip()  +
    theme(axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) 
  
  if(!is.null(coord_cartesian_y)){
    plot_histogram_y <- 
      plot_histogram_y + scale_x_continuous(limits = coord_cartesian_y) + coord_flip()
    plot_scatter <- 
      plot_scatter + scale_y_continuous(limits = coord_cartesian_y)
  }
  
  
  entire_plot <- 
    plot_histogram_y + plot_scatter + plot_spacer() + plot_histogram_x +
    # plot_layout(widths = c(.2,1), heights = c(1,.2)) +
    plot_annotation(title = plot_title)
  
  if(plot_to_return == "all"){
    return(entire_plot)
  } else if (plot_to_return == "scatter") {
    return(plot_scatter)
  }
}



#####################################################################################


plot_two_slopes_for_two_source_and_one_news_station <- function(
    df_regression_betas_wide,
    slope_to_plot,
    sources_to_plot,
    news_station_first_to_plot,
    coord_cartesian_x = NULL,
    coord_cartesian_y = NULL
){
  
  num_bins <- 30
  
  sources_to_investigate = c("helpful", "random", "opposite", "bluebias")
  colour_plot_x <- 
    custom_colours$source_types[which(sources_to_investigate == sources_to_plot[1])]
  colour_plot_y <- 
    custom_colours$source_types[which(sources_to_investigate == sources_to_plot[2])]
  
  alpha_level_for_0_point = .3
  
  df_subset <- df_regression_betas_wide %>% 
    filter(news_station_first == news_station_first_to_plot)
  
  variables_to_plot <- paste0(
    slope_to_plot, "_" ,sources_to_plot
  )
  
  plot_title <- paste0(
    "Comparing ", sources_to_plot[1], " and ", sources_to_plot[2], " source\n",
    slope_to_plot, " (News Station ", news_station_first_to_plot, ")"
  )
  
  plot_scatter <- df_subset %>% 
    ggplot(aes_string(x = variables_to_plot[1], variables_to_plot[2])) +
    geom_hline(yintercept = 0, alpha = alpha_level_for_0_point,
               linetype = "dashed") +
    geom_vline(xintercept = 0, alpha = alpha_level_for_0_point,
               linetype = "dashed") +
    geom_smooth(method='lm', alpha = .2, colour = "grey") +  
    geom_point(fill = "lightgrey",
               colour = "black",
               shape = 21,
               size = 4,
               alpha = .9) +
    stat_cor()
  
  plot_histogram_x <- df_subset %>% 
    ggplot(aes_string(variables_to_plot[1])) +
    geom_histogram(
      colour = colour_plot_x,
      fill = colour_plot_x,
      bins = num_bins,
      position = "dodge", 
      alpha = .3,
      aes(y = ..density..)) +
    geom_vline(xintercept = 0, alpha = alpha_level_for_0_point) +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
  
  if(!is.null(coord_cartesian_x)){
    plot_histogram_x <- 
      plot_histogram_x + coord_cartesian(xlim = coord_cartesian_x)
    plot_scatter <- 
      plot_scatter + scale_x_continuous(limits = coord_cartesian_x)
  }
  
  plot_histogram_y <- df_subset %>% 
    ggplot(aes_string(variables_to_plot[2])) +
    geom_histogram(
      colour = colour_plot_y,
      fill = colour_plot_y,
      bins = num_bins,
      position = "dodge", 
      alpha = .3,
      aes(y = ..density..)) +
    geom_vline(xintercept = 0, alpha = alpha_level_for_0_point) +
    coord_flip()  +
    theme(axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) 
  
  if(!is.null(coord_cartesian_y)){
    plot_histogram_y <- 
      plot_histogram_y + scale_x_continuous(limits = coord_cartesian_y) + coord_flip()
    plot_scatter <- 
      plot_scatter + scale_y_continuous(limits = coord_cartesian_y)
  }
  
  entire_plot <- 
    plot_histogram_y + plot_scatter + plot_spacer() + plot_histogram_x +
    plot_layout(widths = c(.2,1), heights = c(1,.2)) +
    plot_annotation(title = plot_title)
  
  return(entire_plot)
}
