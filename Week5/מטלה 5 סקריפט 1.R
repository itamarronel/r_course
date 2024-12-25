# R course for beginners
# Week 5 Assignment by < Itamar > < Ronel >, id < 032702391 >

#### SCRIPT 1 - DATA FRAME  ----

#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Ctrl + L to clear the console.

#load data and packeges
oad_or_install <- function(packages) 
{for (pkg in packages) 
{if (!require(pkg, character.only = TRUE)) 
{install.packages(pkg, dependencies = TRUE)
library(pkg, character.only = TRUE)}}}

required_packages <- c("dplyr", "ggplot2", "tidyr", "ggpubr", "effectsize")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")
load("./data/df2.rdata")

#### CREATE DATAFRAME ----

# Generate vectors
set.seed(123)
N = 40
subject_id = seq(from = 1, to = N, by = 1)
age = runif(N, 18, 60)
gender = factor(sample(c('male', 'female'), size = N, replace = TRUE),
                levels = c('male', 'female')) 
group = factor(sample(c("control", "clinical"), size = N, replace = TRUE),
               levels = c("control", "clinical"))
treatment = factor(sample(c("placebo", "TMS"), size = N, replace = TRUE),
                   levels = c("placebo", "TMS"))
# Generate memory scores from a truncated normal distribution between 0 and 100
memory = qnorm(runif(N, 
                     pnorm(0, mean = 80, sd = 10),
                     pnorm(100, mean = 80, sd = 10)), 
               mean = 80, sd = 10)

# Create data frame
df2 = data.frame(subject_id, age, gender, group, treatment, memory)

# Ensure directory exists
if (!dir.exists("./data")) { dir.create("./data") }

# Print current working directory
print(getwd())

# Save data frame
write.csv(df2, file = "./data/df2.csv", row.names = FALSE)
save(df2, file = "./data/df2.rdata")

# Check the first few rows of the data frame
head(df2)
