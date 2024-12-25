

#### SCRIPT 2 - FILTERED DATA ----

#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Cmd + Shift + F10 (for mac)
# Ctrl + L to clear the console.

# Load required packages
required_packages <- c("dplyr")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")

# Load the raw data file
load("raw_data.RData")

# Check the structure of the loaded data
glimpse(df)

#### CALCULATE RETAINED PERCENTAGES ----
# Calculate original trials per subject
original_trial_counts <- df |>
  group_by(subject) |>
  summarise(original_trials = n()) |>
  ungroup()

# Filter the data: remove NA and unreasonable reaction times
df_filtered <- df |>
  filter(!is.na(rt), !is.na(accuracy)) |>
  filter(rt >= 300, rt <= 3000)

# Check for remaining NA values and out-of-bounds reaction times
validation_checks <- df_filtered |>
  summarise(
    na_in_rt = sum(is.na(rt)),
    na_in_accuracy = sum(is.na(accuracy)),
    out_of_bounds_rt = sum(rt < 300 | rt > 3000)
  )

# View the validation results 
glimpse(df_filtered)
# Check for remaining NA values and out-of-bounds rt (should all be zero)
print(validation_checks)

#### SUMMARY STATISTICS ----
# Calculate retained trial percentages for each subject
retained_trial_counts <- df_filtered |>
  group_by(subject) |>
  summarise(retained_trials = n()) |>
  ungroup()

# Merge original and retained counts
trial_summary <- original_trial_counts |>
  left_join(retained_trial_counts, by = "subject") |>
  mutate(
    retained_percent = (retained_trials / original_trials) * 100
  )

# Calculate summary statistics
summary_stats <- trial_summary |>
  summarise(
    num_subjects = n_distinct(subject),
    mean_retained = mean(retained_percent, na.rm = TRUE),
    sd_retained = sd(retained_percent, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)

# SAVE FILTERED DATA 
# Save the filtered data to an RData file
save(df_filtered, file = "filtered_data.RData")
cat("Filtered data saved successfully to 'filtered_data.RData'\n")