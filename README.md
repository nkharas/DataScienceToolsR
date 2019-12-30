# DataScienceToolsR
This repository provides tools in R that establishes a framework for data scientists to build, diagnose and evaluate several modeling techniques, and enable them to select the best data models. The framework provides several approaches to try different data transformation and feature engineering tecniques, and diagnostic metrics to evaluate the best performing models against holdout data. Currently, the framework supports regression for continuous variables. Support for classification problems is currently in progress.

## Data Transformations
These functions apply different types of data transformations to raw data. These are particularly useful for linear regression, where normality is desired, but the actual data is not normal. These functions apply transformations like log and square root, and provide additional methods to deal with negative values to implement these transformations correctly.

## Model Evaluation
These functions help data scientists build and evaluate models in R. 
* The model building framework uses several techniques such as simple linear regression, forward stepping, L1 and L2 regularization methods (lasso and ridge regression). 
* The model evaluation framework uses several techniques such as MSE, model R-squared, and prediction rank R-squared against holdout data.
