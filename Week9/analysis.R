# R Course: Data Analysis Script
# Week 9 Assignment by Itamar Ronel, ID 032702391
# Script: analysis.R

#### PREPARE WORKSPACE ----
# Clean the environment
rm(list = ls())
cat("\014")  # Clear the console

# Load required packages
library(dplyr)
library(tidyr)

#### CREATE DATA FRAME ----
# Generate and save the data frame for analysis

# Setting a seed for reproducibility
set.seed(123)

# Define the number of subjects
n_subjects <- 200  # Change this to any number of subjects

# Creating the data frame
data <- data.frame(
  subject_id = 1:n_subjects,  # Subject ID
  age = sample(18:80, n_subjects, replace = TRUE),  # Age sampled uniformly
  gender = sample(c("Male", "Female"), n_subjects, replace = TRUE, prob = c(0.5, 0.5)),  # Gender sampled uniformly
  reaction_time = round(rgamma(n_subjects, shape = 2, scale = 1500)),  # Reaction time (Gamma distribution)
  depression = round(rnorm(n_subjects, mean = 50, sd = 20)),  # Depression (Normal distribution)
  sleep_hours = round(rnorm(n_subjects, mean = 7, sd = 2))  # Average sleep hours (Normal distribution)
)

# Ensuring values are within specified ranges
data$reaction_time[data$reaction_time < 200] <- 200
data$reaction_time[data$reaction_time > 6000] <- 6000
data$depression[data$depression < 0] <- 0
data$depression[data$depression > 100] <- 100
data$sleep_hours[data$sleep_hours < 2] <- 2
data$sleep_hours[data$sleep_hours > 12] <- 12

# Display the data frame
print(data)

#### RUN DESCRIPTIVE STATISTICS ----
# Use the descriptive statistics function
source("functions.R")

# Case 1: Fewer than 10 rows AFTER filtering
tryCatch({
  # This should trigger "Data is too short" because the filtered range contains fewer than 10 rows
  analysis <- generate_statistics(data, subject_start = 12, subject_end = 15)
  print(analysis)
}, error = function(e) {
  print(e$message)  # Print the error message
})

# Case 2: Valid range with more than 10 rows
tryCatch({
  analysis <- generate_statistics(data, subject_start = 100, subject_end = 120)
  print(analysis)
}, error = function(e) {
  print(e$message)
})
