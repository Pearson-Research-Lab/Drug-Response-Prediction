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

continuous_vars <- c("Age","Baseline_HbA1c","BMI", "Total_Chol", ......."Diff_HbA1c")  # Replace with the names of your continuous variables
df_scaled[continuous_vars] <- lapply(df_scaled[continuous_vars], min_max_scaling)
                
# Train-Test Split
train_index <- createDataPartition(df_scaled$Response_Group, p = 0.7, list = FALSE)
train_data_lm <- selected_data[train_index, ]
test_data_lm <- selected_data[-train_index, ]


                               
