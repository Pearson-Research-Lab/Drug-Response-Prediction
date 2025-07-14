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
lm(Diff_HbA1c ~ ., data = df_log)

# Min-Max Scaling (for ML models)
min_max_scaling <- function(x) ...
df_scaled[continuous_vars] <- lapply(..., min_max_scaling)

# Train-Test Split
train_index <- createDataPartition(...)
train_data_lm <- selected_data[train_index, ]
test_data_lm <- selected_data[-train_index, ]
