

#### SCRIPT 4 - INFERENTIAL STATISTICS ----

#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Cmd + Shift + F10 (for mac)
# Ctrl + L to clear the console.

# Load required packages using the custom function
required_packages <- c("dplyr", "lme4", "BayesFactor")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")

# Load the filtered data
load("filtered_data.RData")

#### CLASSICAL MIXED-EFFECTS REGRESSION (lme4) ----
# Fit the linear mixed-effects model
rt_model <- lmer(rt ~ task * congruency + (1 | subject), data = df_filtered)

# Summary of the classical model
cat("### Classical Linear Mixed-Effects Model:\n")
summary(rt_model)

# Applying log-transformation to rt to address rt skewed distribution
# Fit the classical linear mixed-effects model - log
rt_model_log <- lmer(log(rt) ~ task * congruency + (1 | subject), data = df_filtered)

# Summary of the classical model - log
cat("### Classical Linear Mixed-Effects Model - log:\n")
summary(rt_model_log)

#### MOVING TO SIMPLER ANALYSES DUE TO SINGULARITY ISSUES ####
# The boundary (singular) fit warning indicates insufficient variance 
# to justify random effects. We'll shift to simpler analyses: 
# 1. Fixed-effects models (lm) without random effects.
# 2. Bayesian analysis using BayesFactor for model comparison.

# FIXED-EFFECTS MODEL (lm)
# Fit a linear model without random effects
lm_model <- lm(rt ~ task * congruency, data = df_filtered)

# Summary of the linear model
cat("### Fixed-Effects Model Summary:\n")
summary(lm_model)

# BAYESIAN ANALYSIS USING BAYESFACTOR 
# Run a Bayesian analysis without random effects
bf_model <- anovaBF(
  formula = rt ~ task * congruency + subject,
  data = df_filtered,
  whichRandom = "subject", # Define 'subject' as a random factor
  iterations = 10000 # Number of iterations for Bayesian analysis
)

# View the Bayesian model results
cat("### Bayesian Model Summary:\n")
print(bf_model)
