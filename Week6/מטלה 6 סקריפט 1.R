# R course for beginners
# Week 6 Assignment by < Itamar > < Ronel >, id < 032702391 >

#### SCRIPT 1 - DATA FRAME  ----

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

# Generate data for 20 participants
set.seed(123) # Set seed for reproducibility
N <- 20


# Variables
participant_id <- 1:N
age <- runif(N, min = 18, max = 60)
gender <- sample(c("Male", "Female"), N, replace = TRUE)
stress <- runif(N, min = 0, max = 10)
satisfaction = rnorm(N, mean = 50 - 3 * stress, sd = 25)

# Create a data frame
df <- data.frame(
  ParticipantID = participant_id,
  Age = age,
  Gender = gender,
  Stress = stress,
  Satisfaction = satisfaction
)

# Ensure directory exists
if (!dir.exists("./data")) { dir.create("./data") }

# Print current working directory
print(getwd())

# Save data frame
write.csv(df, file = "./data/df.csv", row.names = FALSE)
save(df, file = "./data/df.rdata")

