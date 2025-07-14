# --- Residualize Baseline HbA1c against Diff_HbA1c ---
model <- lm(Diff_HbA1c ~ Baseline_HbA1c, data = selected_data)
res_Baseline <- residuals(model)

# Create a residuals column and clean up
selected_data$Baseline_HbA1c <- NULL
selected_data$Diff_HbA1c <- NULL
selected_data$Res_Baseline <- res_Baseline[match(rownames(selected_data), names(res_Baseline))]

full_model <- lm(Res_Baseline ~ ., data = selected_data)
summary(full_model)
