getwd()
setwd("D:/R work")


# Variable understanding
# AKI                            0=no         1=yes
# Anesthesia methods             0=General anesthesia combined with nerve block        1=General anesthesia
# Emergency surgery              0=no         1=yes
# Chronic Kidney Disease (CKD)   0=no         1=yes
# Liver cirrhosis                0=no         1=yes
# Diabetes                       0=no         1=yes  
# Coronary heart disease         0=no         1=yes
# Preoperative antihypertensive medication    0=no        1=yes
# Heparin during admission       0=no         1=yes
# Intraoperative norepinephrine  0=no 1=yes

# Perform multiple imputation

library(readr)
data <- read_excel("AKI.xlsx")

# Install and load the mice package
install.packages("mice")
library(mice)

# Check missing values in the dataset
summary(data)
md.pattern(data)

# Use mice for multiple imputation
imputed_data <- mice(data, m = 5, method = 'pmm', seed = 500)
summary(imputed_data)

# Extract the first imputed dataset
complete_data_1 <- complete(imputed_data, 1)

# Export the first imputed dataset to a CSV file
write.xlsx(complete_data_1, "complete_data_1.xlsx", row.names = FALSE)

# Export all imputed datasets as separate CSV files
for (i in 1:5) {  # m=5 indicates 5 imputed datasets
  imputed_data_i <- complete(imputed_data, i)  # Get the i-th imputed dataset
  write.csv(imputed_data_i, paste0("complete_data_", i, ".csv"), row.names = FALSE)
}

# Visualize imputation results
stripplot(imputed_data, pch = 20, cex = 1.2)

# Split data into training and validation sets at 70:30 ratio

# Randomly split the file by proportion
library(readr)
mydata <- read_excel("complete_data_1.xlsx")
install.packages("caret")
library(caret)
set.seed(123)
trianandvad <- createDataPartition(y = mydata$ID, p = 0.70, list = FALSE)
train <- mydata[trianandvad, ]
vadi <- mydata[-trianandvad, ] 
write.xlsx(train, "train.xlsx")
write.xlsx(vadi, "vadi.xlsx")
# Read data
library(readr)
train <- read_excel("train.xlsx")
library(readr)
vadi <- read_excel("vadi.xlsx")

# LASSO regression for variable selection
library(glmnet)
library(readxl)
library(plyr)
library(caret)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(openxlsx)
x <- as.matrix(train[,-c(1)])# Remove the first row (dependent variable), x should have no missing values, otherwise use dtcom<-na.omit(dtcom) to remove missing values
y <- as.double(train$A)# Set dependent variable
fit <- glmnet(x, y, family = "binomial", nlambda = 1000, alpha = 1)
View(fit)
print(fit)
plot(fit, xvar = "lambda")# Create plot
lasso_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1, type.measure = "auc", nlambda = 1000)# Build equation for variable selection, with two lines: lambda.min (left) and lambda.lse (right), choose based on actual situation.
plot(lasso_fit)
print(lasso_fit)
lasso_best <- glmnet(x = x, y = y, alpha = 1, lambda = lasso_fit$lambda.1se)# Choose lambda.min or lambda.lse based on the number of variables
coef(lasso_best)
coefficient <- coef(lasso_best, s = lasso_best$lambda.min)
coe <- coefficient@x
coe <- as.data.frame(coe)
Active_Index <- which(as.numeric(coefficient) != 0)
active_coefficients <- as.numeric(coefficient)[Active_Index]
variable <- rownames(coefficient)[Active_Index]
variable <- as.data.frame(variable)
variable <- cbind(variable, coe)
View(variable)# Display final selected variables and regression coefficients
write.csv(variable, "variable.csv", row.names = FALSE)

# Baseline table comparison
# Read data
library(readr)
data <- read_excel("complete_data_1")

# Install tableone and dplyr packages (if not installed)
install.packages("tableone")
install.packages("dplyr")

# Load packages
library(tableone)
library(dplyr)

# Ensure A=AKI is a factor, grouped by dependent variable
data$A <- factor(data$A, levels = c("0", "1"))# Can also group by ID numbers of train and vadi

# Select baseline features for comparison
vars <- c("Age", "Gender", "Diabetes", "Hypertension", "BMI")# Variables can be modified

# Create baseline table, stratified by Group column
baseline_table <- CreateTableOne(vars = vars, strata = "A", data = data, test = TRUE)

# View the generated baseline table
print(baseline_table)

# Export baseline table to CSV file
write.csv(as.data.frame(baseline_table), "baseline_table.csv", row.names = TRUE)

# Fractional polynomial
# Load necessary packages
library(mfp)  # For fp()
# Apply FP transformation to variables in training set train
train$C <- fp(train$C)        # Apply fractional polynomial to C
train$AH <- fp(train$AH)      # Apply fractional polynomial to AH
train$DS <- fp(train$DS)      # Apply fractional polynomial to DS

train$CT <- fp(train$CT)      # Apply fractional polynomial to CT
train$DF <- fp(train$DF)      # Apply fractional polynomial to DF
train$DP <- fp(train$DP)      # Apply fractional polynomial to DP

# Apply FP transformation to variables in validation set vadi
vadi$C <- fp(vadi$C)          # Apply fractional polynomial to C
vadi$AH <- fp(vadi$AH)        # Apply fractional polynomial to AH
vadi$DS <- fp(vadi$DS)        # Apply fractional polynomial to DS

vadi$CT <- fp(vadi$CT)        # Apply fractional polynomial to CT
vadi$DF <- fp(vadi$DF)        # Apply fractional polynomial to DF
vadi$DP <- fp(vadi$DP)        # Apply fractional polynomial to DP

# Build model with selected variables
fml <- as.formula(A == 1 ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ )
modelA <- glm(fml, data = train, family = binomial)

# Prepare regression equation parameters

glm3 <- summary(modelA)
glm3


# Prepare multivariate analysis results for publication format
glm3$coefficients

OR <- round(exp(glm3$coefficients[,1]), 2)
OR


SE <- round(glm3$coefficients[,2], 3)
CI2.5 <- round(exp(coef(modelD)-1.96*SE), 2)
CI97.5 <- round(exp(coef(modelD)+1.96*SE), 2)
CI <- paste0(CI2.5,'-',CI97.5)
B <- round(glm3$coefficients[,1], 3)
Z <- round(glm3$coefficients[,3], 3)
P <- round(glm3$coefficients[,4], 3)

# Create data frame
mlogit <- data.frame( 
  'B' = B,
  'SE' = SE,
  'OR' = OR,
  'CI' = CI,
  'Z' = Z,
  'P' = P)[-1,]   # Remove constant term
mlogit

# Remove the first row (constant term), if [-1,] is not used above, the constant term will be generated, leading to merging issues
# mlogit <- mlogit[-1,] 

View(mlogit)

mlogit

View(mlogit)

# Write multivariate analysis results to CSV, create regression coefficient table
write.csv(mlogit, "multi.csv")

# Discrimination (AUC) for training and validation sets

fml <- as.formula(A == 1 ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ )

modelA <- glm(fml, data = train, family = binomial(logit))


# Calculate predicted values in modeling population
train$predmodelA <- predict(newdata = train, modelA, "response")
View(train)

# Calculate predicted values in validation population
vadi$predmodelA <- predict(newdata = vadi, modelA, "response")
View(vadi)

library(pROC)

# AUC and ROC analysis for training set, modelA
devmodelA <- roc(A ~ predmodelA, data = train, smooth = F)

devmodelA

round(auc(devmodelA), 3)
round(ci(auc(devmodelA)), 3)


# Draw ROC curve
plot(devmodelA, print.auc = TRUE, print.thres = TRUE, main = "ROC CURVE", 
     col = "blue", print.thres.col = "blue", identity.col = "blue",
     identity.lty = 1, identity.lwd = 1)


## AUC and ROC analysis for validation set, modelA
devmodelA <- roc(A ~ predmodelA, data = vadi, smooth = F)
round(auc(devmodelA), 3)
round(ci(auc(devmodelA)), 3)


# Draw ROC curve
plot(devmodelA, print.auc = TRUE, print.thres = TRUE, main = "ROC CURVE", 
     col = "red", print.thres.col = "red", identity.col = "red",
     identity.lty = 1, identity.lwd = 1)

# Hosmer-Lemeshow test
# install.packages("calibrate")
library(calibrate)
library(MASS)
# install.packages("rms")
library(rms)



# Perform Hosmer-Lemeshow test in modeling population
source("HLtest.R") # Ensure HLtest.R is placed in the previously set working directory.
hl.ext2(train$predmodelA, train$A)

# Perform Hosmer-Lemeshow test in validation population
source("HLtest.R") # Ensure HLtest.R is placed in the previously set working directory.
hl.ext2(vadi$predmodelA, vadi$A)


# Plot calibration curve

# Load necessary packages
library(riskRegression)
library(rms)

# Global settings: use common sans-serif font to avoid Unicode character support issues
par(family = "sans")

# 1. Data preprocessing (for train dataset)
required_vars <- c("A", "EQ", "F", "DP", "R", "FJ", "C", "CT", "I", 
                   "DF", "DW", "J", "L", "DS", "AH", "AQ", "AB", "P", "BO")
train_clean <- na.omit(train[, required_vars])
train_clean$A <- as.factor(train_clean$A)

# 2. Fit logistic regression main model
formula <- A ~ EQ + F + DP + R + FJ + C + CT + I + DF + DW + J + L + DS + AH + AQ + AB + P + BO
fit1 <- glm(formula, data = train_clean, family = binomial())

# 3. Performance evaluation: ROC and Brier (optional to retain)
set.seed(123)
xb <- Score(
  list(fit = fit1),
  formula    = A ~ 1,
  data       = train_clean,
  null.model = FALSE,
  plots      = "ROC",
  metrics    = c("auc", "brier"),
  B          = 1000,
  cens.model = NULL
)

# 4. Calculate calibration intercept & slope
train_clean$lp <- predict(fit1, type = "link")
calib_model <- glm(as.numeric(A) - 1 ~ lp, data = train_clean, family = binomial())
coef_vals <- coef(calib_model)
ci_vals   <- confint(calib_model)

intercept <- coef_vals[1]
slope     <- coef_vals[2]
int_ci    <- ci_vals[1, ]
slope_ci  <- ci_vals[2, ]

# 5. Generate bootstrap‐corrected calibration curve using rms package
dd <- datadist(train_clean)
options(datadist = 'dd')
lrm_mod <- lrm(A ~ lp, data = train_clean, x = TRUE, y = TRUE)
cal <- calibrate(lrm_mod, method = 'boot', B = 1000)

# 6. Plot calibration curve and add annotations
plot(cal,
     xlim   = c(0, 1), ylim = c(0, 1),
     xlab   = 'Predicted probability',
     ylab   = 'Observed probability',
     legend = FALSE)
abline(0, 1, lty = 2)  # 45° ideal line

# Add intercept & slope annotations to the plot (use ordinary "-")
text(
  x      = 0.65, 
  y      = 0.15,
  labels = sprintf(
    "Intercept = %.3f (95%% CI %.3f-%.3f)\nSlope     = %.3f (95%% CI %.3f-%.3f)",
    intercept, int_ci[1], int_ci[2],
    slope,     slope_ci[1], slope_ci[2]
  ),
  adj = c(0, 0),
  cex = 0.8
)

# Add legend description (also use ordinary "-")
legend(
  "bottomright",
  legend = "Bootstrap-corrected (B=1000)",
  bty    = "n"
)
# Create correlation heatmap
# Install necessary packages (if not installed)
install.packages("corrplot")
install.packages("ggplot2")

# Load necessary packages
library(corrplot)
library(ggplot2)
library(car)

# Assume train is the training dataset
# Select predictor variables retained in the final model
variables <- train[, c("C", "F", "I", "J", "L", "P", "R", "AB", "AH", "AQ", "BO", "CT", "DF", "DP", "DS", "DW", "EQ", "FJ")]

# Generate correlation matrix between predictor variables
cor_matrix <- cor(variables, use = "complete.obs")  # Adjust use parameter based on actual data

# Calculate VIF and tolerance values
# Assume A is the dependent variable
model <- lm(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ, data = train)
vif_values <- car::vif(model)
tolerance_values <- 1 / vif_values

# Output VIF and tolerance values
print(vif_values)
print(tolerance_values)

# Generate correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addrect = 2, 
         cl.ratio = 0.15, cl.align.c = "center", 
         diag = FALSE, title = "Correlation Matrix of Predictors",
         tl.cex = 1.2, cl.cex = 1.2, rect.col = "darkgrey")
# Create DCA (Decision Curve Analysis)
library(rmda)

### Plot DCA curve

model_1 <- decision_curve(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ,
                          data = train,
                          family = binomial(logit),
                          thresholds = seq(0, 1, by = 0.01),
                          confidence.intervals = 0.95,
                          study.design = 'case-control',
                          population.prevalence = 0.3)
model_2 <- decision_curve(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ,
                          data = vadi,
                          family = binomial(logit),
                          thresholds = seq(0, 1, by = 0.01),
                          confidence.intervals = 0.95,
                          study.design = 'case-control',
                          population.prevalence = 0.3)
# Plot the curve
plot_decision_curve(model_1, curve.names = c('Model'),
                    xlim = c(0, 0.8),
                    cost.benefit.axis = FALSE,
                    col = c('blue'),
                    confidence.intervals = FALSE,
                    standardize = FALSE)


plot_decision_curve(model_2, curve.names = c('Model'),
                    xlim = c(0, 0.8),
                    cost.benefit.axis = FALSE,
                    col = c('red'),
                    confidence.intervals = FALSE,    # TRUE to show confidence intervals
                    standardize = FALSE)

# NRI (Net Reclassification Improvement)
# Package preparation: need to install install.packages("nricens") first
# install.packages("nricens")
library(survival)
library(nricens)
library(rms)
library(foreign)


# Build regression models

devmodelA <- glm(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ 
                 , data = train, family = binomial(link = "logit"), x = TRUE)

devmodelB <- glm(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF, data = train, 
                 family = binomial(link = "logit"), x = TRUE)


# Calculate NRI

# Calculate continuous NRI, define reclassification when value is 5%
library(nricens)

NRI <- nribin(mdl.std = devmodelA, mdl.new = devmodelB, updown = 'diff', cut = 0.05, niter = 500, alpha = 0.05)
# Note: if the plotting area is not large enough, no plot will be shown, and a red warning will be displayed

# Calculate NRI for categorical variables, define reclassification when probability exceeds 0.48 (0.48 is the cutoff determined by previous ROC analysis of model C)

NRI <- nribin(mdl.std = devmodelA, mdl.new = devmodelB,
              updown = 'category', cut = 0.48, niter = 500, alpha = 0.05)

# Sensitivity analysis: before vs after imputation

# Load necessary packages
install.packages("mice")
install.packages("pROC")
install.packages("caret")
install.packages("e1071")
library(mice)
library(pROC)
library(caret)
library(e1071)

# Assume you already have train and vadi datasets, and model with A as dependent variable

# Define model variables
vars <- c("C", "F", "I", "J", "L", "P", "R", "AB", "AH", "AQ", "BO", "CT", "DF", "DP", "DS", "DW", "EQ", "FJ")

# Perform imputation for missing values using mice
imputed_train <- mice(train[vars], m = 5, method = "pmm", seed = 500)  # Impute training set
imputed_vadi <- mice(vadi[vars], m = 5, method = "pmm", seed = 500)    # Impute validation set

# Extract the first imputed dataset
complete_train_1 <- complete(imputed_train, 1)
complete_vadi_1 <- complete(imputed_vadi, 1)

# Define function to calculate model evaluation metrics
calculate_metrics <- function(model, test_data, test_labels) {
  # Predict results
  predictions <- predict(model, newdata = test_data, type = "response")
  
  # Convert predictions to binary classification (threshold=0.5)
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate confusion matrix
  cm <- confusionMatrix(factor(predicted_class), factor(test_labels))
  
  # Get AUC, accuracy, sensitivity, specificity, F1 score
  auc_value <- roc(test_labels, predictions)$auc
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  f1 <- cm$byClass['F1']
  
  return(c(AUC = auc_value, Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, F1_Score = f1))
}

# Sensitivity analysis: preoperative variables model VS complete variables model

# Load required libraries
library(caret)
library(pROC)

# Assume training and validation set data are train and vadi respectively
# Preoperative variables and complete variables
pre_vars <- c("C", "F", "I", "J", "L", "P", "R", "AB", "AH", "AQ", "CT", "DW", "EQ")
full_vars <- c("C", "F", "I", "J", "L", "P", "R", "AB", "AH", "AQ", "BO", "CT", "DF", "DP", "DS", "DW", "EQ", "FJ")

# Build the first model: use preoperative variables
model_train_pre <- glm(AKI ~ ., data = train[, c(pre_vars, "AKI")], family = "binomial")
pred_train_pre <- predict(model_train_pre, train[, pre_vars], type = "response")
pred_vadi_pre <- predict(model_train_pre, vadi[, pre_vars], type = "response")

# Build the second model: use complete variables
model_train_full <- glm(AKI ~ ., data = train[, c(full_vars, "AKI")], family = "binomial")
pred_train_full <- predict(model_train_full, train[, full_vars], type = "response")
pred_vadi_full <- predict(model_train_full, vadi[, full_vars], type = "response")

# Convert to binary classification predictions
pred_train_pre_class <- ifelse(pred_train_pre > 0.5, 1, 0)
pred_vadi_pre_class <- ifelse(pred_vadi_pre > 0.5, 1, 0)

pred_train_full_class <- ifelse(pred_train_full > 0.5, 1, 0)
pred_vadi_full_class <- ifelse(pred_vadi_full > 0.5, 1, 0)

# Calculate AUC, accuracy, sensitivity, specificity, F1 score
# For Train Dataset
roc_train_pre <- roc(train$AKI, pred_train_pre)
roc_train_full <- roc(train$AKI, pred_train_full)

# For Validation Dataset
roc_vadi_pre <- roc(vadi$AKI, pred_vadi_pre)
roc_vadi_full <- roc(vadi$AKI, pred_vadi_full)

# Evaluation Metrics Function
eval_metrics <- function(true_labels, pred_labels) {
  cm <- confusionMatrix(as.factor(pred_labels), as.factor(true_labels))
  auc <- roc(true_labels, pred_labels)$auc
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  f1 <- cm$byClass['F1']
  return(c(AUC = auc, Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, F1 = f1))
}

# Train Data Results
train_results_pre <- eval_metrics(train$AKI, pred_train_pre_class)
train_results_full <- eval_metrics(train$AKI, pred_train_full_class)

# Validation Data Results
vadi_results_pre <- eval_metrics(vadi$AKI, pred_vadi_pre_class)
vadi_results_full <- eval_metrics(vadi$AKI, pred_vadi_full_class)

# Output results
results <- data.frame(
  Model = c("Preoperative Model (Train)", "Full Model (Train)", "Preoperative Model (Validation)", "Full Model (Validation)"),
  AUC = c(train_results_pre['AUC'], train_results_full['AUC'], vadi_results_pre['AUC'], vadi_results_full['AUC']),
  Accuracy = c(train_results_pre['Accuracy'], train_results_full['Accuracy'], vadi_results_pre['Accuracy'], vadi_results_full['Accuracy']),
  Sensitivity = c(train_results_pre['Sensitivity'], train_results_full['Sensitivity'], vadi_results_pre['Sensitivity'], vadi_results_full['Sensitivity']),
  Specificity = c(train_results_pre['Specificity'], train_results_full['Specificity'], vadi_results_pre['Specificity'], vadi_results_full['Specificity']),
  F1 = c(train_results_pre['F1'], train_results_full['F1'], vadi_results_pre['F1'], vadi_results_full['F1'])
)

# Display results
print(results)

# Grouped calibration (e.g. deciles of predicted risk) showing observed versus predicted AKI rates

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(rms)

# Assume your training set is train and validation set is vadi
# Datasets include dependent variable A and independent variables C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ

# Fit logistic regression model (based on training set)
model <- lrm(A ~ C + F + I + J + L + P + R + AB + AH + AQ + BO + CT + DF + DP + DS + DW + EQ + FJ, 
             data = train)

# Predict AKI risk in training and validation sets
train$predicted_risk <- predict(model, type = "fitted")
vadi$predicted_risk <- predict(model, newdata = vadi, type = "fitted")

# Merge training and validation sets for grouped calibration
combined_data <- bind_rows(train, vadi)

# 1. Group predicted AKI risk probabilities into deciles
combined_data$decile <- ntile(combined_data$predicted_risk, 10)

# 2. Calculate observed and predicted AKI rates for each group
grouped_calibration <- combined_data %>%
  group_by(decile) %>%
  summarise(
    observed_AKI_rate = mean(A),  # Observed AKI rate
    predicted_AKI_rate = mean(predicted_risk)  # Predicted AKI rate
  )

# 3. Plot comparison of observed and predicted AKI rates
ggplot(grouped_calibration, aes(x = decile)) +
  geom_line(aes(y = observed_AKI_rate), color = "blue", size = 1) +  # Observed rate (blue solid line)
  geom_line(aes(y = predicted_AKI_rate), color = "red", size = 1, linetype = "dashed") +  # Predicted rate (red dashed line)
  labs(x = "Risk Decile", y = "AKI Rate", 
       title = "Grouped Calibration: Observed vs Predicted AKI Rates") +
  theme_minimal()