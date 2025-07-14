# Load & Prepare the Data

df <- read.delim(...) 

# Relevel & Factorize Categorical Variables

df$Ave_Dose <- relevel(..., ref = "Low")
categorical_vars <- c("Gender", ..., "Treatment_Group")
df[categorical_vars] <- lapply(..., as.factor)

# Select Variables for Modeling
selected_vars_3 <- c("Age", ..., "Diff_HbA1c")
df_filtered <- df[selected_vars_3]

# Export Descriptive Stats
table1(~ ., data = SU_df_filtered)
write.xlsx(...)

# Log Transformation of Select Variables
var_log <- c("BMI", "Total_Chol", ...)
df_log[var_log] <- lapply(..., log)

# Initial Full Linear Model
full_model <- lm(Diff_HbA1c ~ ., data = df_log)
summary(full_model)

# Min-Max Scaling (for ML models)
min_max_scaling <- function(x) ...
df_scaled[continuous_vars] <- lapply(..., min_max_scaling)

# Train-Test Split
train_index <- createDataPartition(...)
train_data_lm <- selected_data[train_index, ]
test_data_lm <- selected_data[-train_index, ]

# Model Training and Evaluation
# Linear Regression (CV = 10)
lm_model <- train(..., method = "lm", trControl = trControl_lm)

# Random Forest
rf_model <- train(..., method = "rf", ...)

# Support Vector Machine (SVM)
svm_model <- train(..., method = "svmRadial", ...)

# XGBoost
dmy <- dummyVars(" ~ .", data = selected_data)
trsf <- data.frame(predict(...))

rwCV <- xgb.cv(...)
xgb_model <- xgboost(..., nrounds = 200)

# Artificial Neural Network (ANN)
model <- neuralnet(Diff_HbA1c ~ ., ...)

# Bayesian Additive Regression Trees (BART)
bart_model <- wbart(train_x, train_y, test_x)

# Results Aggregation
results_data <- data.frame("Models" = c(...), "RMSE", "MAE", "R_Squared")












