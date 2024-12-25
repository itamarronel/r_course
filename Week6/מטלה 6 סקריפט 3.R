#### SCRIPT 3 - REGRESSION ANALYSIS ----

# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Cmd + Shift + F10 (for mac)
# Ctrl + L to clear the console.

# Load required packages
required_packages <- c("dplyr")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")

# Load data
load("./data/df.rdata")
cat("Data loaded successfully.\n")

#### REGRESSION MODEL ----

# Perform regression analysis
model <- lm(Satisfaction ~ Stress, data = df)

# Print the summary of the model
cat("\n--- Regression Summary - 200 subjects ---\n")
summary(model)

#--- Regression Summary - 20 subjects ---
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   60.029      8.631   6.955 1.69e-06 ***
#  Stress        -4.571      1.850  -2.471   0.0237 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 21.3 on 18 degrees of freedom
# Multiple R-squared:  0.2533,	Adjusted R-squared:  0.2118 
# F-statistic: 6.106 on 1 and 18 DF,  p-value: 0.02369

#--- Regression Summary - 200 subjects ---
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   50.704      3.587  14.136  < 2e-16 ***
#  Stress        -2.967      0.618  -4.801 3.11e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 25.42 on 198 degrees of freedom
# Multiple R-squared:  0.1043,	Adjusted R-squared:  0.09973 
# F-statistic: 23.05 on 1 and 198 DF,  p-value: 3.113e-06

##### ANSWER QUESTION ----
# תשובה: עם 200 נבדקים אנחנו הרבה יותר קרובים לפרמטרים שהזנו
# הפרמטרים היו 50, -3, 25, ואפשר לראות שעם 200 נבדקים כבר הגענו אליהם בקירוב 

##### PEARSON VS SCALED ----

# Compute Pearson correlation between Stress and Satisfaction
correlation <- cor(df$Stress, df$Satisfaction, method = "pearson")
cat("\n--- Pearson Correlation ---\n")
cat("Correlation (r):", correlation, "\n")

# Standardize variables
df <- df |>
  mutate(Stress_scaled = scale(Stress),
    Satisfaction_scaled = scale(Satisfaction))

# Perform regression with standardized variables
model_scaled <- lm(Satisfaction_scaled ~ Stress_scaled, data = df)

# Print summary of the standardized regression model
cat("\n--- Standardized Regression Summary ---\n")
summary(model_scaled)

# key results:
# Pearson Correlation (r): -0.5032813
# Standardized Regression Coefficients: Slope (b1_scaled): -5.033e-01

# CONCLUSION: THEY ARE THE SAME


#### SHAVUA SHAKET ----

#### BRING THEM HOME -----




