fsi_preparation <- function(accom_price_layer, accom_review_layer, food_safety_layer) {
  lvals_accom_price = c("cut-rate", "affordable", "expensive")
  lvals_accom_review = c("reasonable", "good", "excellent")
  lvals_food_safety = c("low", "medium", "high")

  fsi <- fsi_create("To visit or not to visit, that is the question",
                    default_conseq = genmf("trimf", c(10, 30, 60)))

  # Linguistic variables...
  fsi <- fsi_add_fsa(fsi, "accommodation price", accom_price_layer)
  fsi <- fsi_add_fsa(fsi, "accommodation review", accom_review_layer)
  fsi <- fsi_add_fsa(fsi, "food safety", food_safety_layer)

  # Consequent...
  lvals_visiting_exp <- c("awful", "average", "great")
  awful_mf <- genmf("trimf", c(0, 0, 20))
  average_mf <- genmf("trimf", c(10, 30, 60))
  great_mf <- genmf("trapmf", c(40, 80, 100, 100))
  fsi <- fsi_add_cs(fsi, "visiting experience", lvals_visiting_exp,
                    c(awful_mf, average_mf, great_mf), c(0, 100))

  rules <- c(
    "IF accommodation review is reasonable AND food safety is low THEN visiting experience is awful",
    "IF accommodation price is expensive AND accommodation review is reasonable THEN visiting experience is awful",
    "IF accommodation price is affordable AND accommodation review is good AND food safety is medium THEN visiting experience is average",
    "IF accommodation price is affordable AND accommodation review is excellent AND food safety is high THEN visiting experience is great",
    "IF accommodation price is cut-rate AND accommodation review is excellent AND food safety is high THEN visiting experience is great"
  )

  fsi <- fsi_add_rules(fsi, rules)
  fsi
}

fsi_evaluation <- function(fsi, qw, approach, ...) {

    args_function <- list(...)

    if(approach=="discretization") {
      k <- args_function[["k"]]

      start_time <- Sys.time()
      result <- fsi_qw_eval(fsi, target_lval = "great", qw, approach = "discretization", k = k)
      end_time <- Sys.time()

      total_test_time = difftime(end_time, start_time, units="s")

      df_results_metrics <- tibble(k = k,
                                   start_time = start_time,
                                   end_time = end_time,
                                   elapsed_time = total_test_time)

    } else if(approach=="optimization"){

      max_depth <- args_function[["max_depth"]]
      maxit <- args_function[["maxit"]]
      population <- args_function[["population"]]

      start_time <- Sys.time()
      result <- fsi_qw_eval(fsi, qw, approach = "pso",
                            max_depth = max_depth, maxit = maxit,
                            population = population)
      end_time <- Sys.time()

      total_test_time = difftime(end_time, start_time, units="s")

      df_results_metrics <- tibble(max_depth = max_depth,
                                   maxit = maxit,
                                   population = population,
                                   start_time = start_time,
                                   end_time = end_time,
                                   elapsed_time = total_test_time)
    }

    print(paste0("Time Stats for ", approach, " evaluation."))
    print(df_results_metrics)

    formatted_result <- cbind(st_coordinates(result$points), result$inferred_values)
    colnames(formatted_result) <- c("x", "y", "inferred_value")
    formatted_result <- as_tibble(formatted_result)

    formatted_result
}
