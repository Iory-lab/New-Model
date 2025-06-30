# AKI Prediction Model: Data Preprocessing, Modeling, and Evaluation

## Overview

This repository contains R code and data used for developing and evaluating a predictive model for Acute Kidney Injury (AKI) based on preoperative clinical variables. The pipeline includes data preprocessing, handling of missing values via multiple imputation, feature selection using Lasso regression, model development using logistic regression, and performance evaluation through various metrics, including AUC, sensitivity, specificity, and F1 score. The model’s calibration and clinical utility are assessed using calibration curves and decision curve analysis (DCA), and its improvement over the baseline model is quantified using the Net Reclassification Improvement (NRI).

## Project Goals

* **Data Preprocessing**: Handle missing data using multiple imputation.
* **Feature Selection**: Select relevant features using Lasso regression.
* **Model Development**: Build a logistic regression model using the selected features.
* **Model Evaluation**: Assess the model’s performance using AUC, accuracy, sensitivity, specificity, and F1 score.
* **Model Calibration**: Evaluate the model’s calibration using calibration curves.
* **Clinical Utility**: Use Decision Curve Analysis (DCA) to assess clinical relevance.
* **Model Improvement**: Quantify model improvement using NRI.

## Files Included

1. **`AKI.xlsx`**: Input dataset containing preoperative clinical features and AKI status (0 = no AKI, 1 = AKI).
2. **`complete_data_1.xlsx`**: The first imputed dataset generated using multiple imputation.
3. **`train.xlsx`**: The training dataset (70% of the data).
4. **`vadi.xlsx`**: The validation dataset (30% of the data).
5. **`variable.csv`**: The selected features from Lasso regression along with their coefficients.
6. **`baseline_table.csv`**: Baseline comparison table between the AKI and non-AKI groups.
7. **`multi.csv`**: Logistic regression model coefficients, including odds ratios, confidence intervals, and p-values.
8. **`HLtest.R`**: Custom script for performing the Hosmer-Lemeshow test.

## Methodology

### 1. **Setting up the Working Environment**

```r
getwd()
setwd("D:/R work")  # Update with your working directory path
```

### 2. **Library Installation**

Ensure that the necessary libraries are installed by running the following:

```r
install.packages(c("mice", "glmnet", "caret", "pROC", "rms", "dplyr", "tableone", "corrplot", "rmda"))
```

### 3. **Data Preprocessing: Multiple Imputation**

* The dataset `AKI.xlsx` is read and imputed for missing values using the `mice` package. Multiple imputed datasets are generated, and the first dataset is saved as `complete_data_1.xlsx`.

```r
library(mice)
data <- read_excel("AKI.xlsx")
imputed_data <- mice(data, m = 5, method = 'pmm', seed = 500)
summary(imputed_data)
complete_data_1 <- complete(imputed_data, 1)
write.xlsx(complete_data_1, "complete_data_1.xlsx", row.names = FALSE)
```

### 4. **Data Splitting: Training and Validation Sets**

The data is split into training (70%) and validation (30%) sets using the `caret` package:

```r
library(caret)
set.seed(123)
trianandvad <- createDataPartition(y = mydata$ID, p = 0.70, list = FALSE)
train <- mydata[trianandvad, ]
vadi <- mydata[-trianandvad, ]
write.xlsx(train, "train.xlsx")
write.xlsx(vadi, "vadi.xlsx")
```

### 5. **Feature Selection: Lasso Regression**

Lasso regression is used to perform variable selection. The variables selected by Lasso are stored in `variable.csv`:

```r
x <- as.matrix(train[, -c(1)])  # Remove the outcome variable from features
y <- as.double(train$A)  # Outcome variable
fit <- glmnet(x, y, family = "binomial", nlambda = 1000, alpha = 1)
lasso_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "auc", nlambda = 1000)
lasso_best <- glmnet(x = x, y = y, alpha = 1, lambda = lasso_fit$lambda.1se)
coef(lasso_best)
variable <- as.data.frame(coef(lasso_best, s = lasso_best$lambda.min))
write.csv(variable, "variable.csv", row.names = FALSE)
```

### 6. **Model Development: Logistic Regression**

A logistic regression model is developed using the features selected by Lasso regression:

```r
fml <- as.formula(A == 1 ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ)
modelA <- glm(fml, data = train, family = binomial)
```

### 7. **Model Evaluation: Performance Metrics**

The model is evaluated on both the training and validation sets using AUC, accuracy, sensitivity, specificity, and F1 score. The `pROC` package is used for ROC curve analysis:

```r
devmodelA <- roc(A ~ predmodelA, data = train, smooth = FALSE)
round(auc(devmodelA), 3)
round(ci(auc(devmodelA)), 3)
```

### 8. **Calibration: Hosmer-Lemeshow Test**

The Hosmer-Lemeshow test is used to evaluate the calibration of the model:

```r
source("HLtest.R")
hl.ext2(train$predmodelA, train$A)
```

### 9. **Calibration Plot**

A calibration plot is created to visualize the agreement between predicted probabilities and observed outcomes:

```r
library(rms)
cal <- calibrate(modelA, method = 'boot', B = 1000)
plot(cal, xlim = c(0, 1), ylim = c(0, 1), xlab = 'Predicted probability', ylab = 'Observed probability')
```

### 10. **Clinical Utility: Decision Curve Analysis (DCA)**

Decision Curve Analysis (DCA) is performed to assess the clinical utility of the model:

```r
model_1 <- decision_curve(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ,
                          data = train, family = binomial(logit), thresholds = seq(0, 1, by = 0.01))
plot_decision_curve(model_1, curve.names = c('Model'))
```

### 11. **Net Reclassification Improvement (NRI)**

NRI is calculated to assess the improvement in model performance when adding more predictors:

```r
NRI <- nribin(mdl.std = devmodelA, mdl.new = devmodelB, updown = 'diff', cut = 0.05, niter = 500, alpha = 0.05)
```

### 12. **Grouped Calibration: Deciles of Predicted Risk**

Grouped calibration compares observed versus predicted AKI rates based on deciles of predicted risk:

```r
combined_data$decile <- ntile(combined_data$predicted_risk, 10)
ggplot(grouped_calibration, aes(x = decile)) + 
  geom_line(aes(y = observed_AKI_rate), color = "blue", size = 1) + 
  geom_line(aes(y = predicted_AKI_rate), color = "red", size = 1, linetype = "dashed")
```

---

## Results

The results from the model evaluation, calibration, and clinical utility analysis are stored in CSV files:

* **`variable.csv`**: Variables selected by Lasso regression.
* **`baseline_table.csv`**: Baseline characteristics comparison between AKI and non-AKI groups.
* **`multi.csv`**: Coefficients and odds ratios for the logistic regression model.
* **`DCA_plot.png`**: Decision curve analysis plot.
* **`calibration_plot.png`**: Calibration plot.

## Conclusion

This R script provides a complete workflow for the development, evaluation, and calibration of a predictive model for Acute Kidney Injury. The model's performance and clinical utility have been thoroughly evaluated, ensuring that it provides valuable insights for clinical decision-making.

---

## References

* **Multiple Imputation**: [https://cran.r-project.org/web/packages/mice/mice.pdf](https://cran.r-project.org/web/packages/mice/mice.pdf)
* **Lasso Regression**: [https://glmnet.stanford.edu/](https://glmnet.stanford.edu/)
* **Hosmer-Lemeshow Test**: [https://www.rdocumentation.org/packages/rms/versions/6.2-0/topics/hltest](https://www.rdocumentation.org/packages/rms/versions/6.2-0/topics/hltest)
* **Decision Curve Analysis**: [https://cran.r-project.org/web/packages/rmda/rmda.pdf](https://cran.r-project.org/web/packages/rmda/rmda.pdf)


