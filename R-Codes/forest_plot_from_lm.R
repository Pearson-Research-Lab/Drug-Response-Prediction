# forest_plot_function.R

forest_plot_from_lm <- function(model, term_labels = NULL, x_limits = c(-8, 8), title = "Forest plot of linear regression") {
  library(broom)
  library(dplyr)
  library(ggplot2)

  # Tidy model output with confidence intervals
  summary_table <- tidy(model, conf.int = TRUE)

  # Add significance stars
  summary_table <- summary_table %>%
    mutate(significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )) %>%
    filter(term != "(Intercept)") %>%  # Remove intercept (optional)
    mutate(term = reorder(term, estimate))

  # Subset of significant variables (p < 0.1)
  significant_table <- summary_table %>%
    filter(p.value < 0.1) %>%
    mutate(pval_label = ifelse(p.value < 0.001, "p < 0.001", sprintf("p = %.3f", p.value)))

  # Start plot
  plot <- ggplot(summary_table, aes(x = estimate, y = term)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkblue") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Beta Coefficients", y = "Clinical Variables", title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11, color = "black")
    ) +
    geom_text(data = significant_table, aes(label = paste0(pval_label, significance)), 
              hjust = -0.2, vjust = -0.2, color = "red", size = 3) +
    scale_x_continuous(limits = x_limits, breaks = seq(x_limits[1], x_limits[2], 1))

  # Custom labels for variables if provided
  if (!is.null(term_labels)) {
    plot <- plot + scale_y_discrete(labels = term_labels)
  }

  return(plot)
}

#######################################
# Example usgae

# Load model
lm_model <- lm(outcome ~ var1 + var2 + var3, data = your_data)

# Optional: create label list to rename variables
labels <- c(
  "log_Total_Chol" = "log(Total cholesterol)",
  "log_BMI" = "log(BMI)",
  "Smoking_StatusYes" = "Smoking (Yes)"
)

# Call function
forest_plot_from_lm(model = lm_model, term_labels = labels)
