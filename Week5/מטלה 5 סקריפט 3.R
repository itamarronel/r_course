#### SCRIPT 3 - DATA ANALYSIS ----

# Load data & packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "ggpubr")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")
load("./data/df2.rdata")

#### REGRESSION ANALYSIS ----

# a. Verify contrasts 
group_contrast <- matrix(c(0, 1), ncol = 1)
rownames(group_contrast) <- c("control", "clinical")
contrasts(df2$group) <- group_contrast

treatment_contrast <- matrix(c(0, 1), ncol = 1)
rownames(treatment_contrast) <- c("placebo", "TMS")
contrasts(df2$treatment) <- treatment_contrast

# Print contrasts to verify
print("Group Contrasts:")
print(contrasts(df2$group))

print("Treatment Contrasts:")
print(contrasts(df2$treatment))

# b. Regression analysis
model <- lm(memory ~ group * treatment, data = df2)

# c. Results
print("Model Coefficients:")
print(model$coef)

print("Full Model Summary:")
print(summary(model))

