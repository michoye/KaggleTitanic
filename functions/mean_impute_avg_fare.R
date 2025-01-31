mean_impute_avg_fare <- function(data) {
  source("functions/calculate_avg_fare.R")
  
  # Calculate average fares for patients with Fare == 0 for each Pclass 
  avg_fares <- calculate_avg_fare(data)
  
  # Impute 0 Fare
  data <- data |> 
    left_join(avg_fares, by = "Pclass") |> 
    mutate(Fare = case_when(
      Fare != 0 ~ Fare, 
      Fare == 0 ~ avg_fare
    ))
  
  return(data)
}
