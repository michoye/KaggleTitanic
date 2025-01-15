mean_impute_missing_age <- function(data) {
  source("functions/calculate_age_means.R")
  source("functions/calculate_age_means2.R")
  source("functions/create_title_groups.R")
  
  # Add title groups
  data_with_titles <- create_title_groups(data)
  
  # Calculate mean ages for each title group and passenger class
  group_means_1 <- calculate_age_means(data_with_titles)
  group_means_2 <- calculate_age_means2(data_with_titles)
  
  # Impute missing ages
  data_with_titles <- data_with_titles |>
    left_join(group_means_1, by = c("title_group", "Pclass")) |>
    left_join(group_means_2, by = c("title_group", "Pclass", "Parch")) |>
    mutate(Age = case_when(
      !is.na(Age) ~ Age,
      is.na(Age) & title_group != "Miss" ~ mean_age_1,
      is.na(Age) & title_group == "Miss" ~ mean_age_2
    ))
  
  return(data_with_titles)
}