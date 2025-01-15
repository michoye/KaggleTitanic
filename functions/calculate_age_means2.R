# Function to calculate mean ages by title and class and Parch
calculate_age_means2 <- function(data) {
  data |> 
    filter(!is.na(Age)) |>
    group_by(title_group, Pclass, Parch) |>
    summarise(mean_age_2 = mean(Age), .groups = "drop")
}