# R course for beginners
# Week 10 Assignment by < Itamar > < Ronel >, id < 032702391 >
# Script: Data coding & Di
#### SCRIPT 1 - DATA FRAME & DESCRIPTIVE STATISTICS ----

#### PREPARE WORKSPACE ----
# Clean the environment
rm(list = ls())
cat("\014")  # Clear the console

# Load required packages
library(dplyr)
library(tidyr)

# Set Working Directory path
setwd("/Users/itamar/Documents/GitHub/r_course")  

# Load dataset 
titanic <- read.csv("data/titanic.csv")

# Check column names and head
names(titanic)
head(titanic)

#### PREPARE DATA FRAME ----

# Rename column 'Sex' to 'Gender'
titanic <- rename(titanic, Gender = Sex)

# Convert 'Gender' to a factor and set "male" as the reference level
titanic$Gender <- factor(titanic$Gender, levels = c("male", "female"))

# Check the structure of the 'Gender' variable
str(titanic$Gender)

# Ensure Pclass is treated as a character before applying ifelse()
titanic$Class <- factor(ifelse(as.character(titanic$PClass) == "1st", "1st", "Other"), 
                              levels = c("1st", "Other"))

# Check the distribution
table(titanic$Class)

#### DESCRIPTIVE STATISTICS ----

# Load required package
library(dplyr)

# Summarize data by Class & Gender
desc_stats <- titanic %>%
  group_by(Class, Gender) %>%
  summarise(
    total_passengers = n(),  
    survivors = sum(Survived),  
    non_survivors = total_passengers - survivors,
    survival_rate = mean(Survived),
    sd_survival = sd(Survived)
  ) %>%
  ungroup()

# Summarize total passengers and survival per Class (ignoring Gender)
class_stats <- titanic %>%
  group_by(Class) %>%
  summarise(
    total_passengers = n(),
    survivors = sum(Survived),
    non_survivors = total_passengers - survivors,
    survival_rate = mean(Survived),
    sd_survival = sd(Survived)
  ) %>%
  ungroup()

# Print results
print("Descriptive Statistics by Class & Gender:")
print(desc_stats)

print("Overall Descriptive Statistics by Class:")
print(class_stats)



