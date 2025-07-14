# shap_summary_function.R

generate_shap_summary_plot <- function(xgb_model, X_train, custom_labels = NULL,
                                       file_path = NULL, width = 8, height = 5, res = 600,
                                       plot_type = c("sina", "wrap1")) {
  library(SHAPforxgboost)
  library(dplyr)
  library(ggplot2)

  # Match plot type
  plot_type <- match.arg(plot_type)

  if (plot_type == "sina") {
    # Compute SHAP values and prepare long format
    shap_values <- shap.values(xgb_model = xgb_model, X_train = X_train)
    shap_long <- shap.prep(xgb_model = xgb_model, X_train = X_train)

    # Apply custom labels if provided
    if (!is.null(custom_labels)) {
      shap_long <- shap_long %>%
        mutate(variable = recode(variable, !!!custom_labels))
    }

    # Create plot
    p <- shap.plot.summary(shap_long, kind = "sina")
    if (!is.null(custom_labels)) {
      p <- p + scale_y_discrete(labels = custom_labels)
    }

  } else if (plot_type == "wrap1") {
    # wrap1 method handles plotting internally
    p <- NULL  # No plot object returned
  }

  # Save to TIFF if path is provided
  if (!is.null(file_path)) {
    tiff(file_path, units = "in", width = width, height = height, res = res)
    if (plot_type == "wrap1") {
      shap.plot.summary.wrap1(xgb_model, X_train)
    } else {
      print(p)
    }
    dev.off()
  }

  # Return ggplot object if available
  if (!is.null(p)) {
    return(p)
  } else {
    return(invisible(NULL))  # For wrap1: no object returned
  }
}

#####################################################################################################
# Example usage
# Labels for cleaner plot axes
labels <- c(
  "Total_Chol" = "Total cholesterol",
  "BMI" = "BMI",
  "Smoking_Status.Unknown" = "Smoking (Unknown)",
  "Time_to_TrtStart" = "Time to treatment start",
  "Gender.M" = "Gender (Male)"
)

# Sina plot with label renaming and file export
generate_shap_summary_plot(
  xgb_model = xgb_model,
  X_train = train_x,
  custom_labels = labels,
  file_path = "shap_sina_plot.tiff",
  plot_type = "sina"
)
