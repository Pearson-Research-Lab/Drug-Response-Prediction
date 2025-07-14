library(randomForest)
library(caret)
library(dplyr)
library('odbc')
library('tidyverse')
library('dbplyr')
library(DBI)
library(lubridate)
library(tidyverse)
library(broom)
library(pROC)
library(pdp)
library(e1071)
library(xgboost)
library(ggplot2)
library(Metrics)
library(ROCR)
library(BART)

# Load & Prepare the Data
df <- read.delim("inputfile.txt", header=T) 
df$Response_Group <- ifelse(df$Treatment_HbA1c <= 58 & df$Diff_HbA1c < 0, "Yes", "No")

# Relevel & Factorize Categorical Variables
df$Ave_Dose <- relevel(as.factor(df$Ave_Dose), ref = "Low")
categorical_vars <- c("Gender", "hb_code", "Ethnicity", "Ave_Dose", "Smoking_Status", "SIMD", "T2D_Duration", "Treatment_Group", "Response_Group")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# Select Variables for Modeling
selected_vars_3 <- c("Age", ..., "Response_Group")
df_filtered <- df[selected_vars_3]

# Log Transformation of Select Variables
var_log <- c("BMI", "Total_Chol", ...)
df_filtered[var_log] <- lapply(df_filtered[var_log], function(x) log(x))

# Initial Full Linear Model
full_model <- lm(Response_Group ~ ., data = df_filtered)
summary(full_model)

# Min-Max Scaling (for ML models)
df_scaled <- df_filtered
min_max_scaling <- function(x) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}

continuous_vars <- c("Age","Baseline_HbA1c","BMI", "Total_Chol", ......)  # Replace with the names of your continuous variables
df_scaled[continuous_vars] <- lapply(df_scaled[continuous_vars], min_max_scaling)
                
# Train-Test Split
train_index <- createDataPartition(df_scaled$Response_Group, p = 0.7, list = FALSE)
train_data_lm <- selected_data[train_index, ]
test_data_lm <- selected_data[-train_index, ]

###########################################################
# Model Training and Evaluation
# Linear Regression (CV = 10)
set.seed(42)
ctrlspecs <- trainControl(method="cv", 
                          number=10, 
                          savePredictions="all",
                          summaryFunction = twoClassSummary,
                          classProbs=TRUE)
# Create a logistic regression model
logit_model <- train(Response_Group ~ ., 
                     data = train_data_lm,
                     method = "glm", 
                     family = binomial, trControl=ctrlspecs)

logit_probs <- predict(logit_model, test_data_lm, type = "prob")
## Compute ROC curve and AUC for the logistic regression model
logit_roc <- roc(test_data_lm$Response_Group, logit_probs[, "Yes"], direction = "<")
logit_auc <- logit_roc$auc
logit_ci <- as.numeric(ci(logit_roc))
## Create a data frame with the ROC curve coordinates
logit_roc_df <- data.frame(Specificities = logit_roc$specificities, 
                           Sensitivities = logit_roc$sensitivities,
  FPR = 1-logit_roc$specificities, TPR = logit_roc$sensitivities,
  model_name = "Logistic Regression",
  auc = round(logit_auc, 4))
                               
############################################################
# Random Forest
set.seed(142)
ctrlspecs <- trainControl(method="cv", number=10)
set.seed(42)
tuneGrid <- expand.grid(.mtry = 4)
rf_model <- train(Response_Group~.,
    data = train_data_lm,
    method = "rf",
    tuneGrid = tuneGrid,
    trControl = ctrlspecs,
    importance = TRUE,
    nodesize = 15,
    ntree = 600)
## Predict probabilities using the logistic regression model on test data
rf_probs <- predict(rf_model, test_data_lm, type = "prob")
# Compute ROC curve and AUC for the logistic regression model
rf_roc <- roc(test_data_lm$Response_Group, rf_probs[, "Yes"], direction = "<")
rf_auc <- rf_roc$auc
## Calculate confidence intervals
rf_ci <- as.numeric(ci(rf_roc))

#############################################################
# Support Vector Machine (SVM)
set.seed(42)
ctrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)
svm_model <- train(Response_Group ~ .,       
  data = train_data_lm,               
  method = "svmRadial",
  trControl = ctrl)
##Predict using SVM
predYsvm = predict(svm_model, test_data_lm)
## Predict probabilities using the svm  model on test data
svm_probs <- predict(svm_model, test_data_lm, type = "prob")
## Compute ROC curve and AUC for the svm  model
svm_roc <- roc(test_data_lm$Response_Group, svm_probs[, "Yes"], direction = "<")
svm_roc_auc <- svm_roc$auc
## Calculate confidence intervals
svm_ci <- as.numeric(ci(svm_roc))
                          
###############################################################
# XGBoost
# Convert categorical variables to dummy variables using one-hot encoding
dmy <- dummyVars(" ~ .", data = df_scaled, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = df_scaled))  ## think about xgboost again, split and onehot encoding both
set.seed(42)
# Split the dataset into training and testing sets (70% training, 30% testing)
train_index <- createDataPartition(trsf$Response_Group.Yes, p = 0.7, list = FALSE)
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
# Set hyperparameters (you can adjust these)
params <- list(
  objective = "binary:logistic",  # For binary classification
  eta = 0.1,  # Learning rate
  max_depth = 3,  # Maximum depth of trees
  min_child_weight = 1,           # Minimum sum of instance weight (Hessian) needed in a child
  subsample = 0.7,                  # Subsample ratio of the training instances
  colsample_bytree = 0.7,           # Subsample ratio of column
  nrounds = 200)  # Number of boosting rounds
# Train an XGBoost model
xgb_model <- xgboost(
  data = dtrain,
  params = params,
  nthread = 2,
  nrounds = params$nrounds)
# Make predictions
xgb_predicted_values <- predict(xgb_model, dtest)

xgb_pred_obj <- ROCR::prediction(xgb_predicted_values, test_data_x$Response_Group.Yes)

## Create a performance object for AUC-ROC calculation
xgb_perf <- ROCR::performance(xgb_pred_obj, "tpr", "fpr")
## Calculate the AUC-ROC value
xgb_auc <- ROCR::performance(xgb_pred_obj, "auc")
auc_value_xgb <- unlist(slot(xgb_auc, "y.values"))
xgb_ci <- as.numeric(ci(roc_curve_xgb))
                               
##################################################################
# Artificial Neural Network (ANN)
library(neuralnet)
library(sigmoid)
library(neuralnet)
library(sigmoid)
ann_model <- neuralnet(
    Response_Group.Yes ~ .,
    data=train_data_xgb,
    hidden = c(3),
    stepmax = 1e+09,
    threshold = 0.1,
    linear.output = FALSE,
    act.fct = relu)
## Make predictions on the test set
ann_probs <- predict(ann_model, test_data_xgb)
## Compute ROC curve and AUC for the logistic regression model
roc_value_ann <- roc(test_data_x$Response_Group.Yes, ann_probs[, 1], direction = "<")
auc_value_ann <- roc_value_ann$auc
## Evaluate the model performance
pred_class <- ifelse(ann_probs > 0.5,1, 0)
## Evaluate the model performance
confusion_matrix <- confusionMatrix(as.factor(pred_class[,1]), as.factor(test_data_x$Response_Group.Yes))
accuracy <- confusion_matrix$overall['Accuracy']
precision <- confusion_matrix$byClass['Pos Pred Value']
recall <- confusion_matrix$byClass['Sensitivity']
ann_ci <- as.numeric(ci(roc_value_ann))
                               
###################################################################
# Bayesian Additive Regression Trees (BART)
set.seed(99)
library(BART)
bart_model <- wbart(train_x, train_y, test_x)
## Compute ROC curve and AUC for the logistic regression model
roc_value_bart <- roc(test_data_x$Response_Group.Yes, bart_model$yhat.test.mean, direction = "<")
auc_value_bart <- roc_value_bart$auc
## Evaluate the model performance
pred_class <- ifelse(bart_model$yhat.test.mean > 0.5,1, 0)
## Evaluate the model performance
confusion_matrix <- confusionMatrix(as.factor(pred_class), as.factor(test_data_x$Response_Group.Yes))
accuracy <- confusion_matrix$overall['Accuracy']
precision <- confusion_matrix$byClass['Pos Pred Value']
recall <- confusion_matrix$byClass['Sensitivity']
bart_ci <- as.numeric(ci(roc_value_bart))

###################################################################
# Use tests to find differences between models
resamps <- resamples(list(LM = lm_model, SVM = svm_model, RF = rf_model))
diffValues <- diff(resamps)
summary(diffValues)


                               
