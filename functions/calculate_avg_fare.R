# Calculate average fare

calculate_avg_fare <- function(data) {
  data |> 
    filter(Fare != 0) |> 
    group_by(Pclass) |> 
    summarise(avg_fare = mean(Fare), .groups = "drop")
}