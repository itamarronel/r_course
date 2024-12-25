

# R course for beginners
# Week 7 Assignment by < Itamar > < Ronel >, id < 032702391 >

#### SCRIPT 1 - DATA FRAME ----

#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Cmd + Shift + F10 (for mac)
# Ctrl + L to clear the console.

# Create Load function
load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE) } } }

# Load required packages
required_packages <- c("dplyr")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")

# Define the directory containing the data files
data_dir <- "~/Documents/stroop_data" # Path to the stroop_data folder

# List all CSV files in the directory
files <- dir(data_dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize the data frame with the first file
df <- read.csv(files[1])

# Loop through the remaining files and append them
for (i in 2:length(files)) {
  current_file <- read.csv(files[i])
  df <- rbind(df, current_file)
}

# Add 'task' and 'congruency' variables
df <- df |>
  mutate(
    task = ifelse(grepl("word_reading", condition), "word_reading", "ink_naming"),
    congruency = ifelse(grepl("_cong$", condition), "congruent", "incongruent")
    )

# Add the 'accuracy' variable as a logical variable
df <- df |>
  mutate(accuracy = as.logical(participant_response == correct_response))

# Filter relevant columns and convert variable types
df <- df %>%
  select(subject, task, congruency, block, trial, accuracy, rt) %>%
  mutate(
    subject = as.factor(subject),
    task = as.factor(task),
    congruency = as.factor(congruency),
    block = as.factor(block),
    trial = as.numeric(trial),
    accuracy = as.logical(accuracy),
    rt = as.numeric(rt)
  )

# Recode factors for regression
df <- df %>%
  mutate(
    task = relevel(task, ref = "word_reading"), # Set "word_reading" as the reference level
    congruency = relevel(congruency, ref = "congruent") # Set "congruent" as the reference level
  )

# View the levels of the factors
levels(df$task)
levels(df$congruency)

# View the updated data frame structure
str(df)
head(df, 20)


# Save the data frame as an RData file
save(df, file = "raw_data.RData")
