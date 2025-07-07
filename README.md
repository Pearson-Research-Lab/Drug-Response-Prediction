# Drug-Response-Prediction
Comparative evaluation of machine learning models for predicting drug response using clinical variables

# Glycaemic Response Prediction to Sulfonylurea Therapy in Type 2 Diabetes

This repository contains all R code and supporting files used in our study comparing the predictive performance of five machine learning models and a linear regression model to predict glycaemic response (change in HbA1c) following sulfonylurea therapy in individuals with type 2 diabetes.

The analysis aimed to evaluate whether advanced machine learning (ML) models offer any predictive advantage over traditional linear regression when using only routinely collected clinical data. Our results indicate limited gains from ML methods, highlighting the dominant role of baseline HbA1c and the need for richer data sources.

## Repository Structure
├── InputData/
│ └── cleaned_data.csv # Preprocessed clinical dataset

├── R-Codes/
│ └── clean_transform_data.R # Data filtering, variable selection, transformation, 
│ ├── linear_model.R # Linear regression model
│ ├── bart_model.R # BART model
│ ├── xgboost_model.R # XGBoost model
│ ├── random_forest_model.R # Random Forest model
│ ├── svm_model.R # Support Vector Machine model
│ └── model_comparison.R # RMSE, R², AUC comparison
├── shap/
│ ├── shap_xgboost.R # SHAP plot for XGBoost
│ └── shap_bart.R # SHAP-style analysis for BART
├── plots/
│ ├── forest_plot_pps.R # Forest plot for PPS betas/p-values
│ ├── meta_analysis_plot.R # Meta-analysis effect size visualization
│ └── performance_plots.R # RMSE and AUC bar plots


