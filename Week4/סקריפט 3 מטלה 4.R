#### SCRIPT 3 - DATA ANALYSIS ----

#### PREPARE WORKSPACE & LOAD DATA ----

# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Ctrl + L to clear the console.

# Load required libraries
library(dplyr)  
library(ggplot2) 
library(ggdist)  
library(effectsize)  # לחישוב Cohen's d

# Load data 
load("./data/df.rdata")

# VERIFY FACTORS
# Convert gender and depression to factors if not already
df$gender <- factor(df$gender, levels = c("female", "male"))
df$depression <- factor(df$depression, levels = c("no", "yes"))

# Quick check of the data
glimpse(df)   
head(df) 
str(df)
colSums(is.na(df))                      
df <- df[complete.cases(df), ]         

### DATA ANALYSIS ----

# 1. T-test for sleep hours compared to 3 hours ----
t_test_1 <- t.test(df$sleep_hours, mu = 3, alternative = "two.sided")
print(t_test_1)

# 2. Independent t-test for sleep hours by depression ----
t_test_2 <- t.test(sleep_hours ~ depression, data = df, alternative = "two.sided")
print(t_test_2)

# Calculate Cohen's d using the effectsize package
cohens_d_effectsize <- effectsize::cohens_d(sleep_hours ~ depression, data = df)
print(cohens_d_effectsize)

# Calculate Cohen's d manually
group_means <- df |> group_by(depression) |> summarise(mean = mean(sleep_hours), var = var(sleep_hours), n = n())
sd_pooled <- sqrt((group_means$n[1] * group_means$var[1] + group_means$n[2] * group_means$var[2]) /
                    (group_means$n[1] + group_means$n[2] - 2))
cohens_d_manual <- (group_means$mean[2] - group_means$mean[1]) / sd_pooled
print(paste("Cohen's d (manual):", round(cohens_d_manual, 3)))

# 3. Regression analysis using lm() ----
regression_model <- lm(sleep_hours ~ depression, data = df)
summary(regression_model)
