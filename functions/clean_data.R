#JS - 2024-12-18
clean_data <- function(data) {
  
  ticket_pattern <- "^(?:(.*) )?([0-9]+)$"
  
  data |>
    # Convert variables to factors
    mutate(
      across(c("Survived", "Pclass", "Sex", "Embarked"), as.factor)
    ) |>
    # Handle empty strings
    mutate(
      Cabin = if_else(Cabin == "", NA, Cabin),
      Embarked = if_else(Embarked == "", NA, Embarked)
    ) |>
    # Extract ticket information 
    mutate(
      ticket_nr = str_replace(Ticket, ticket_pattern, "\\2"),
      ticket_pre = str_replace(Ticket, ticket_pattern, "\\1")
    )
}