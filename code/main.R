load_required_packages <- function(x) {
  for(i in x){
    if(!require(i, character.only = TRUE)) {
      # we are working on the next version of the fsr that would be not compatible with this code (i.e., with the .rds files)
      # thus, please make sure that you will use the correct version indicated below
      if(i == "fsr") {
        print("Make sure that you are installing the version 1.0.2 of the package fsr")
        remotes::install_version("fsr", "1.0.2", dependencies = TRUE)
      } else {
        install.packages(i, dependencies = TRUE)
      }
      require(i, character.only = TRUE)
    }
  }
}

# First, we need to install/load all required packages
load_required_packages(c("this.path", "readr", "tibble", "dplyr", "sf", "fsr", "ggplot2", "stringr", "FuzzyR"))

current_dir <- this.path::this.dir()
# Loading needed underlying functions
source(file.path(current_dir, "fsi_models.R"))

#################################
## Definition of Query Windows ##

# These query windows were created by using the script available at https://github.com/accarniel/FESTIval/wiki/Default-Datasets
# We generated three different sizes of query windows that is proportional to the total extent of the New York City:
# 2 windows with 0.1% of the total extent
# 2 windows with 1% of the total extent
# 2 windows with 5% of the total extent

qws <- data.frame(id = 1:6, percentage = c(0.1, 0.1, 1, 1, 5, 5))
qws$geom <- c("POLYGON((-74.0079405378277 40.7186870348335,-74.0079405378277 40.7319453532162,-73.9903720963167 40.7319453532162,-73.9903720963167 40.7186870348335,-74.0079405378277 40.7186870348335))",
              "POLYGON((-74.2385985378277 40.5161390348335,-74.2385985378277 40.5293973532162,-74.2210300963167 40.5293973532162,-74.2210300963167 40.5161390348335,-74.2385985378277 40.5161390348335))",
              "POLYGON((-73.8841044621292 40.8428439520083,-73.8841044621292 40.8847704360414,-73.8285481720152 40.8847704360414,-73.8285481720152 40.8428439520083,-73.8841044621292 40.8428439520083))",
              "POLYGON((-74.0177934621292 40.6097709520083,-74.0177934621292 40.6516974360414,-73.9622371720152 40.6516974360414,-73.9622371720152 40.6097709520083,-74.0177934621292 40.6097709520083))",
              "POLYGON((-73.8738811377085 40.6641359598471,-73.8738811377085 40.7578864282027,-73.7496534964359 40.7578864282027,-73.7496534964359 40.6641359598471,-73.8738811377085 40.6641359598471))",
              "POLYGON((-73.9218261377085 40.662540959847,-73.9218261377085 40.7562914282027,-73.7975984964359 40.7562914282027,-73.7975984964359 40.662540959847,-73.9218261377085 40.662540959847))")
qws <- st_as_sf(qws, wkt = "geom")

qw_list <- qws$geom

#######################
## Preparation Phase ##
#######################

# Reading the layers stored in the .RDS files
# These layers were previously created by using the running example of the following paper:
# Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package.
# In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (ACM SIGSPATIAL 2021), pp. 526-535, 2021.
# doi: https://doi.org/10.1145/3474717.3484255

accom_price_layer <- readRDS(file.path(current_dir, "..", "data", "accom_price_layer.rds"))
accom_review_layer <- readRDS(file.path(current_dir, "..", "data", "accom_review_layer.rds"))
food_safety_layer <- readRDS(file.path(current_dir, "..", "data", "food_safety_layer.rds"))

fsi <- fsi_preparation(accom_price_layer, accom_review_layer, food_safety_layer)

#######################
##  Evaluation Phase ##
#######################

############################
##  Discretization Method ##
############################

k_list <- c(100, 10000)

discret_result <- tibble("qw" = integer(),
                         "x" = numeric(),
                         "y" = numeric(),
                         "inferred_value" = numeric(),
                         "k" = integer())

id <- 0
for(qw_num in qw_list){
  id <- id + 1
  for(k in k_list){
    result <- fsi_evaluation(fsi, qw_num, "discretization", k = k)
    result$k <- k
    result$qw <- id
    discret_result <- bind_rows(discret_result, result)
  }
}

##########################
##  Optimization Method ##
##########################

# We set a seed here because of the pso algorithm so that we can obtain the same
# results in the optimization method. In the Code Ocean, we have put the original
# results obtained in our comparative analysis (which may not employed to the same
# seed here).

set.seed(1234)

max_depth_list <- 1:4

optim_result <- tibble("qw" = integer(),
                       "x" = numeric(),
                       "y" = numeric(),
                       "inferred_value" = numeric(),
                       "max_depth" = integer(),
                       "max_it" = integer(),
                       "pop" = integer())

id <- 0
for(qw_num in qw_list){
  id <- id + 1
  for(max_depth in max_depth_list){
    result <- fsi_evaluation(fsi, qw_num, "optimization", maxit = 10, population = 10, max_depth = max_depth)
    result$qw <- id
    result$max_it <- 10
    result$pop <- 10
    result$max_depth <- max_depth
    optim_result <- bind_rows(optim_result, result)
  }
}


########################
##   Result Analysis  ##
########################

# We have the results of our comparative analysis saved in .csv files.
# Therefore, here, we simply read them so that we can reproduce the figures of the
# analysis discussed in the paper. However, if you execute the analysis on your machine,
# you should comment these lines. Hence, the code below would use the results obtained
# from your own experiments.

discret_result <- read_csv(file.path(current_dir, "..", "data", "discretization_results.csv"))
optim_result <- read_csv(file.path(current_dir, "..", "data", "optimization_results.csv"))

stats_discretization <- discret_result %>% group_by(k, qw) %>%
  summarise(
    n_points = n(),
    mean = mean(inferred_value),
    max = max(inferred_value),
    min = min(inferred_value),
    sd = sd(inferred_value),
    var = var(inferred_value),
    n_points_max = sum(inferred_value == max),
    perc_points_max = round(n_points_max/n_points * 100, 2)
  )


stats_optimization <- optim_result %>% group_by(max_depth, qw) %>%
  summarise(
    n_points = n(),
    mean = mean(inferred_value),
    max = max(inferred_value),
    min = min(inferred_value),
    sd = sd(inferred_value),
    var = var(inferred_value),
    n_points_max = sum(inferred_value == max),
    perc_points_max = round(n_points_max/n_points * 100, 2)
  )


###############################################################################
##          General heat maps to show the relationship between               ##
##   the number of points and the average inferred value for each method     ##
###############################################################################

stats_discretization$k_formatted <- str_trim(format(stats_discretization$k, big.mark = ","))
stats_discretization$k_formatted_compl <- paste0("k = ", stats_discretization$k_formatted)


stats_optimization <- stats_optimization %>%
  mutate(max_depth_formatted = paste0("max_depth = ", max_depth))

rng <- c(73, 79)

disc_hm_plot <- ggplot(stats_discretization, aes(as.factor(qw), str_trim(format(k, big.mark = ",")))) +
  geom_tile(color = "black", aes(fill = mean)) +
  geom_label(aes(label = str_trim(format(n_points, big.mark = ",")) ),
             color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "black",
                      limits = rng) +
  coord_fixed() +
  theme_bw() +
  theme(axis.text = element_text(size=11, color = "black"),
        axis.title = element_text(size=12)) +
  labs(y = expression(paste("k"))) +
  labs(x = expression(paste("Query Window"))) +
  labs(fill = "Average Inferred Value")


optim_hm_plot <- ggplot(stats_optimization, aes(as.factor(qw), max_depth)) +
  geom_tile(color = "black", aes(fill = mean)) +
  geom_label(aes(label = n_points),
             color = "black", size=5) +
  scale_fill_gradient(low = "white", high = "black",
                      limits = rng) +
  coord_fixed() +
  theme_bw() +
  theme(axis.text = element_text(size=11, color = "black"),
        axis.title = element_text(size=12)) +
  labs(y = expression(paste("max_depth"))) +
  labs(x = expression(paste("Query Window"))) +
  labs(fill = "Average\nInferred Value")


ggsave(file.path(current_dir, "..", "results", "general_heatmap_discret.png"), plot = disc_hm_plot, dpi = "print", units="cm", width = 20, height = 10)
ggsave(file.path(current_dir, "..", "results", "general_heatmap_optim.png"), plot = optim_hm_plot, dpi = "print", units="cm", width = 13, height = 10)


#############################################################################
## Analyzing the performance of the methods to answer the Optimal RI Query ##
##      That is, how good are the methods to answer this type of RI?       ##
#############################################################################

summarized_df <- stats_discretization
summarized_df$method <- "discretization"
summarized_df$qw <- as.numeric(summarized_df$qw)
summarized_df$k <- NULL
summarized_df$configuration <- summarized_df$k_formatted_compl

stats_optimization$qw <- as.numeric(stats_optimization$qw)
aux_df <- stats_optimization
aux_df$method <- "optimization"
aux_df$configuration <- aux_df$max_depth_formatted
aux_df$max_depth <- NULL

summarized_df <- select(summarized_df, method, n_points, mean, max, n_points_max, perc_points_max, configuration, qw)
aux_df <- select(aux_df, method, n_points, mean, max, n_points_max, perc_points_max, configuration, qw)

summarized_df <- union(summarized_df, aux_df)
summarized_df$qw_formatted <- paste0("query window ", summarized_df$qw)

summarized_df$configuration <- factor(summarized_df$configuration,
                                      levels = c("max_depth = 4", "max_depth = 3",
                                                 "max_depth = 2", "max_depth = 1",
                                                 "k = 10,000", "k = 100"))

#######################################################
## % of total points with the maximum inferred value ##
#######################################################

rate_of_points_maximum_value <- ggplot(summarized_df,
                                       aes(x=perc_points_max, y=configuration))+
  geom_bar(stat = "identity", fill = "black") +
  theme_bw() +
  theme(axis.text = element_text(size=11, color = "black"),
        axis.title = element_text(size=12)) +
  theme(strip.text = element_text(face="bold", size=12)) +
  labs(y = expression(paste("RI methods and their parameters"))) +
  labs(x = expression(paste("% of points with the maximum inferred value"))) +
  facet_wrap(~qw_formatted)

ggsave(file.path(current_dir, "..", "results", "performance_optimal_qwi.png"), plot = rate_of_points_maximum_value, dpi = "print", units="cm", width = 15, height = 10)
