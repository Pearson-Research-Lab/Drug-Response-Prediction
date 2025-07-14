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
df_scaled <- df_log
min_max_scaling <- function(x) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}

continuous_vars <- c("Age","Baseline_HbA1c","BMI", "Total_Chol", ......."Diff_HbA1c")  # Replace with the names of your continuous variables
df_scaled[continuous_vars] <- lapply(df_scaled[continuous_vars], min_max_scaling)
                
# Train-Test Split
train_index <- createDataPartition(df_scaled$Diff_HbA1c, p = 0.7, list = FALSE)
train_data_lm <- selected_data[train_index, ]
test_data_lm <- selected_data[-train_index, ]


###########################################################
# Model Training and Evaluation
# Linear Regression (CV = 10)
trControl_lm = trainControl(method = "cv", number = 10, verboseIter = TRUE)
lm_model <- train(
  Diff_HbA1c ~ . , 
  data=train_data_lm,
  method = "lm",
  trControl = trControl_lm
)
pred_lm <- predict(lm_model, test_data_lm)
mse_lm <- mean((test_data_lm$Diff_HbA1c - pred_lm)^2)
rmse_lm <- sqrt(mse_lm)
mae_lm <- mean(abs(test_data_lm$Diff_HbA1c - pred_lm))
r_squared_lm <- cor(pred_lm, test_data_lm$Diff_HbA1c)^2

############################################################
# Random Forest
set.seed(142)
ctrlspecs <- trainControl(method="cv", number=10)
tuneGrid <- expand.grid(.mtry = 4)
rf_model <- train(Diff_HbA1c~., data = train_data_lm,
    method = "rf", tuneGrid = tuneGrid,
    trControl = ctrlspecs, importance = TRUE,
    nodesize = 15, ntree = 600)
predictions_rf <- predict(rf_model, test_data_lm)
mse <- mean((test_data_lm$Diff_HbA1c - predictions_rf)^2)
rmse_rf <- sqrt(mse)
mae_rf <- mean(abs(test_data_lm$Diff_HbA1c - predictions_rf))
r_squared_rf <- cor(predictions_rf, test_data_lm$Diff_HbA1c)^2

#############################################################
# Support Vector Machine (SVM)
set.seed(42)
ctrl <- trainControl(method = "cv", number = 10)
# Create and train the SVM model
svm_model <- train(Diff_HbA1c ~ .,data = train_data_lm,
  method = "svmRadial",      # Use radial basis kernel SVM
  trControl = ctrl)
predYsvm = predict(svm_model, test_data_lm)
mse <- mean((predYsvm - test_data_lm$Diff_HbA1c)^2)
rmse_svm <- sqrt(mse)
mae_svm <- mean(abs(test_data_lm$Diff_HbA1c - predYsvm))
r_squared_svm <- cor(predYsvm, test_data_lm$Diff_HbA1c)^2

###############################################################
# XGBoost
# Convert categorical variables to dummy variables using one-hot encoding
dmy <- dummyVars(" ~ .", data = df_scaled, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = df_scaled))  ## think about xgboost again, split and onehot encoding both
set.seed(42)
# Split the dataset into training and testing sets (70% training, 30% testing)
train_index <- createDataPartition(trsf$Diff_HbA1c, p = 0.7, list = FALSE)
train_data_x <- trsf[train_index, ]
test_data_x <- trsf[-train_index, ]
train_data_xgb <- as.matrix(train_data_x)
test_data_xgb <- as.matrix(test_data_x)
##define predictor and response variables in training set
train_x = data.matrix(train_data_xgb[, -ncol(train_data_xgb)])
train_y = train_data_xgb[,ncol(train_data_xgb)]
#define predictor and response variables in testing set
test_x = data.matrix(test_data_xgb[, -ncol(test_data_xgb)])
test_y = test_data_xgb[, ncol(test_data_xgb)]
## Convert the data to the DMatrix format for XGBoost
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)
## Set final hyperparameters for the model (you can adjust these)
params <- list(
  objective = "reg:squarederror",  # Regression task
  booster = "gbtree",             # Tree-based boosting
  eta = 0.1,                      # Learning rate
  max_depth = 3,                  # Maximum depth of trees
  min_child_weight = 1,           # Minimum sum of instance weight (Hessian) needed in a child
  subsample = 0.7,                  # Subsample ratio of the training instances
  colsample_bytree = 0.7,           # Subsample ratio of columns
  nrounds = 200                   # Number of boosting rounds (trees)
)
## Train an XGBoost model
xgb_model <- xgboost(
  data = dtrain,
  params = params,
  nrounds = params$nrounds)

## Make predictions
xgb_predicted_values <- predict(xgb_model, dtest)
mse <- mean((xgb_predicted_values - test_y)^2)
rmse_xg <- sqrt(mse)
mae_xg <- mean(abs(test_y - xgb_predicted_values))
r_squared_xg <- cor(xgb_predicted_values, test_y)^2

##################################################################
# Artificial Neural Network (ANN)
library(neuralnet)
library(sigmoid)
# Define the neural network model
model <- neuralnet(
    Diff_HbA1c ~ .,
    data=train_data_xgb,
    hidden = c(2),
    stepmax = 1e+09,
    threshold = 0.1,
    linear.output = FALSE,
    act.fct = relu
)
predictions <- predict(model, test_data_xgb)
# Calculate the evaluation metric (e.g., RMSE)
rmse_nn <- sqrt(mean((predictions - test_data_x$Diff_HbA1c)^2))
mae_nn <- mean(abs(test_data_x$Diff_HbA1c - predictions))
r_squared_nn <- cor(predictions, test_data_x$Diff_HbA1c)^2

###################################################################
# Bayesian Additive Regression Trees (BART)
set.seed(99)
library(BART)
bart_model <- wbart(train_x, train_y, test_x)
bart_pred <- predict(bart_model, as.data.frame(test_x))
rmse_bart <- sqrt(mean((bart_model$yhat.test.mean - test_data_x$Diff_HbA1c)^2))
mae_bart <- mean(abs(test_data_x$Diff_HbA1c - bart_model$yhat.test.mean))
r_squared_bart <- cor(bart_model$yhat.test.mean, test_data_x$Diff_HbA1c)^2











