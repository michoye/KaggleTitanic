mean_impute_missing_age <- function(data) {
  source("functions/calculate_age_means.R")
  source("functions/create_title_groups.R")
  
  # Add title groups
  data_with_titles <- create_title_groups(data)
  
  # Calculate mean ages for each title group and passenger class
  group_means <- calculate_age_means(data_with_titles)
  
  # Impute missing ages
  data_with_titles |>
    left_join(group_means, by = c("title_group", "Pclass")) |>
    mutate(Age = coalesce(Age, mean_age)) |>
    select(-mean_age) # Remove the mean_age column after imputation
}