# Load & Prepare the Data

df <- read.delim("inputfile.txt", header=T) 

# Relevel & Factorize Categorical Variables
df$Ave_Dose <- relevel(as.factor(df$Ave_Dose), ref = "Low")
categorical_vars <- c("Gender", "hb_code", "Ethnicity", "Ave_Dose", "Smoking_Status", "SIMD", "T2D_Duration", "Treatment_Group")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# Select Variables for Modeling
selected_vars_3 <- c("Age", ..., "Diff_HbA1c")
df_filtered <- df[selected_vars_3]

# Export Descriptive Stats
tbl_df <- table1(~ ., data = df_filtered)
openxlsx::write.xlsx(tbl_df, file = "df_summary.xlsx", rowNames = FALSE)

# Log Transformation of Select Variables
var_log <- c("BMI", "Total_Chol", ...)
df_log[var_log] <- lapply(df_log[var_log], function(x) log(x))

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












