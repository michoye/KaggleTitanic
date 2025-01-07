library(ggplot2)
library(dplyr)

# Function for histograms
create_histograms <- function(data, vars, group_var, binwidth = 2, alpha = 0.5) {
  plots <- list()  # To store the generated plots
  
  for (var in vars) {
    # Ensure the variable is numeric
    if (!is.numeric(data[[var]])) {
      warning(paste(var, "is not numeric. Attempting to convert to numeric."))
      data[[var]] <- as.numeric(data[[var]])
    }
    
    # Create the histogram
    p <- data %>%
      filter(!is.na(.data[[group_var]]) & !is.na(.data[[var]])) %>%
      ggplot(aes(x = .data[[var]], fill = factor(.data[[group_var]]), color = factor(.data[[group_var]]))) +
      geom_histogram(
        binwidth = binwidth,
        alpha = alpha,
        position = "identity"
      ) +
      labs(
        title = paste("Histogram of", var, "grouped by", group_var),
        x = var,
        y = "Count",
        fill = group_var,
        color = group_var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
    
    # Store the plot in the list
    plots[[var]] <- p
  }
  
  return(plots)
}

# Function to create bar charts for categorical variables
create_bar_charts <- function(data, vars, group_var, position = "dodge", alpha = 0.7) {
  plots <- list()  # To store the generated plots
  
  for (var in vars) {
    # Check if the variable is categorical
    if (!is.factor(data[[var]]) && !is.character(data[[var]])) {
      warning(paste(var, "is not categorical. Skipping this variable."))
      next  # Skip non-categorical variables
    }
    
    # Create the bar chart
    p <- data %>%
      filter(!is.na(.data[[group_var]]) & !is.na(.data[[var]])) %>%
      ggplot(aes(x = .data[[var]], fill = factor(.data[[group_var]]))) +
      geom_bar(
        position = position,
        alpha = alpha
      ) +
      labs(
        title = paste("Bar Chart of", var, "grouped by", group_var),
        x = var,
        y = "Count",
        fill = group_var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
    
    # Store the plot in the list
    plots[[var]] <- p
  }
  
  return(plots)
}