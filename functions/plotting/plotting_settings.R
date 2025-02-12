# Define general plot style
base_size = 15
theme_set(theme_classic(base_size = base_size))


# Define custom coloururs
custom_colours <- list()
custom_colours$green_probability <- sequential_hcl(5, "Terrain")[1]
custom_colours$blue_probability <- sequential_hcl(5, "YlGnBu")[2]

custom_colours$source_types <- qualitative_hcl(5, palette = "Dark 3")[c(5, 2, 1, 4, 3)]
custom_colours$news_station_first_types <- met.brewer("Peru1", 2)
custom_colours$feedback_condition <- c("purple2", "springgreen3")

custom_colours$blue_green$h <- c(246, 124)
custom_colours$blue_green$c <- 83
custom_colours$blue_green$l <- c(38, 100)

custom_values <- diverge_hcl(8, 
                             custom_colours$blue_green$h,
                             custom_colours$blue_green$c,
                             custom_colours$blue_green$l,power   = 1.5)
custom_values <- custom_values[c(1:3,6:8)]

custom_labels <- list()
custom_labels$source_type <- c("Helpful", "Random", "Opposite", "Blue-biased")


# Labelers for facets
labels_for_facet_grid = list(
  learner_type = list(
    'ground_truth' = "With Feedback",
    'bayes_learner' = "Without Feedback"
  ),
  source_type = list(
    'helpful' = "Helpful",
    'random' = "Random",
    'opposite' = "Opposite",
    'greenbias' = "Green-Bias",
    'bluebis' = "Blue-Bias"    
  ) ,
  trial_third_index = list(
    'first_third' = "First Third",
    'second_third' = "Second Third",
    'third_third' = "Third Third"
  )
)

label_the_feedback_facet_wrap <- function(variable, value){
  return(labels_for_facet_grid[[variable]])
}