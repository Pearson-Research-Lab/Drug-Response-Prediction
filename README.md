# Drug-Response-Prediction

This repository contains all R code and supporting files used in our study comparing the predictive performance of five machine learning models and a linear regression model to predict glycaemic response (change in HbA1c) following sulfonylurea therapy in individuals with type 2 diabetes.

The analysis aimed to evaluate whether advanced machine learning (ML) models offer any predictive advantage over traditional linear regression when using only routinely collected clinical data. Our results indicate limited gains from ML methods, highlighting the dominant role of baseline HbA1c and the need for richer data sources.

## Repository Structure
├── InputData/ # This repository contains a simulated (dummy) DataFrame intended solely to demonstrate the expected input format and structure of the data used in this project.

│ └── input_dataframe.txt  # Please note: The data is entirely fictitious. The purpose of this DataFrame is to clarify the structure, data types, and objects expected in each column. This example can be used as a reference for formatting your own input data.

├── R-Codes/

│ └── Machine_learning_Regression_models.R  # Data filtering, variable selection, transformation, linear regression, Random forest, SVM, XGBoost, ANN, BART 

│ └── Machine_learning_categorical_models.R # Models for binary output

│ └── Residualize_Baseline_HbA1c.R

├── shap/

│ └── generate_SHAP_plot.R # SHAP plot for XGBoost

├── plots/

│ └── forest_plot_from_lm.R # Forest plot for PPS betas/p-values


