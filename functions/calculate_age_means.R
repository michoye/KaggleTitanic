# Function to calculate mean ages by title and class
calculate_age_means <- function(data) {
  data |> 
    filter(!is.na(Age)) |>
    group_by(title_group, Pclass) |>
    summarise(mean_age_1 = mean(Age), .groups = "drop")
}