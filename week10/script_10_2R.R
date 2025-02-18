# R course for beginners
# Week 10 Assignment by < Itamar > < Ronel >, ID < 032702391 >
# Script: Logistic Regression Model Comparison

#### SCRIPT 2 - LOGISTIC REGRESSION MODELS ----

#### PREPARE WORKSPACE ----
# Clean the environment
rm(list = ls())
cat("\014")  # Clear the console

# Load required packages
library(dplyr)
library(pROC)
library(ggplot2)

# Load dataset (assumes Script 1 was already run)
titanic <- read.csv("data/titanic.csv")

# Reapply factor transformations from Script 1
titanic <- titanic %>%
  rename(Gender = Sex) %>%
  mutate(
    Gender = factor(Gender, levels = c("male", "female")),
    Class = factor(ifelse(as.character(PClass) == "1st", "1st", "Other"), levels = c("1st", "Other"))
  )

#### FIT LOGISTIC REGRESSION MODELS ----

# Model 1: Intercept-only
mod1 <- glm(Survived ~ 1, data = titanic, family = binomial())

# Model 2: Intercept + Gender
mod2 <- glm(Survived ~ Gender, data = titanic, family = binomial())

# Model 3: Intercept + Gender + Class
mod3 <- glm(Survived ~ Gender + Class, data = titanic, family = binomial())

#### PRINT MODEL SUMMARIES ----
print("Summary of Model 1 (Intercept-only):")
summary(mod1)

print("Summary of Model 2 (Intercept + Gender):")
summary(mod2)

print("Summary of Model 3 (Intercept + Gender + Class):")
summary(mod3)

#### ROC ANALYSIS ----

# Compute predicted probabilities for each model
pred1 <- predict(mod1, type = "response")
pred2 <- predict(mod2, type = "response")
pred3 <- predict(mod3, type = "response")

# Compute ROC curves
roc1 <- roc(titanic$Survived, pred1)
roc2 <- roc(titanic$Survived, pred2)
roc3 <- roc(titanic$Survived, pred3)

# Compute AUC values
auc1 <- auc(roc1)
auc2 <- auc(roc2)
auc3 <- auc(roc3)

# Print AUC values
print(paste("AUC for Model 1 (Intercept-only):", round(auc1, 3)))
print(paste("AUC for Model 2 (Gender):", round(auc2, 3)))
print(paste("AUC for Model 3 (Gender + Class):", round(auc3, 3)))

#### PLOT ROC CURVES ----

# Create a dataframe for ggplot with the correct FPR & TPR
roc_df <- data.frame(
  FPR = c(1 - rev(roc1$specificities), 1 - rev(roc2$specificities), 1 - rev(roc3$specificities)),
  TPR = c(rev(roc1$sensitivities), rev(roc2$sensitivities), rev(roc3$sensitivities)),
  Model = rep(c("Model 1 (Intercept)", "Model 2 (Gender)", "Model 3 (Gender + Class)"),
              times = c(length(roc1$sensitivities), length(roc2$sensitivities), length(roc3$sensitivities)))
)

# Plot the corrected ROC curves using ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Random baseline
  labs(title = "ROC Curves for Logistic Regression Models",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = "Model") +
  theme_minimal()
