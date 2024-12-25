#### SCRIPT 3 - DESCRIPTIVE STATISTICS WITH PLOTS ----
# Script: Descriptive Statistics and Plots
# Description: This script generates descriptive statistics and visualizations (accuracy, reaction times, histograms) for the Stroop task.
# Author: Itamar Ronel
# Date: 2024-12-25


#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Cmd + Shift + F10 (for mac)
# Ctrl + L to clear the console.

# Load required packages
required_packages <- c("dplyr", "ggplot2")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")

# Load the filtered data
load("filtered_data.RData")

#### DEFINE COLOR INDEX ----
color_index <- list(
  congruency_colors = c("congruent" = "#ffd5ca", "incongruent" = "#fdd89f")
)

#### DESCRIPTIVE STATISTICS ----
# Accuracy by condition
accuracy_by_condition <- df_filtered |>
  group_by(task, congruency) |>
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE) * 100) |> # Convert to percentage
  ungroup()

# Reaction Time Descriptive Stats
rt_stats <- df_filtered |>
  group_by(task, congruency) |>
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE)
  ) |>
  ungroup()

# Print the statistics
print("Accuracy by condition:")
print(accuracy_by_condition)

print("Reaction time descriptive statistics:")
print(rt_stats)

#### PLOTS ----

# Accuracy by condition plot
ggplot(accuracy_by_condition, aes(x = task, y = mean_accuracy, fill = congruency)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = color_index$congruency_colors) +
  geom_text(
    aes(label = sprintf("%.1f%%", mean_accuracy)),
    position = position_dodge(0.9),
    vjust = -0.5
  ) +
  labs(
    title = "Accuracy by Condition",
    x = "Task",
    y = "Mean Accuracy (%)",
    fill = "Congruency"
  ) +
  theme_minimal()

# PLOT REACTION TIME BY TASK AND CONGRUENCY 
ggplot(rt_stats, aes(x = task, y = mean_rt, fill = congruency)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(
    aes(ymin = mean_rt - sd_rt, ymax = mean_rt + sd_rt),
    width = 0.2, position = position_dodge(0.9)
  ) +
  scale_fill_manual(values = color_index$congruency_colors) + # Use colors from the index
  labs(
    title = "Mean Reaction Time by Task and Congruency",
    x = "Task",
    y = "Mean Reaction Time (ms)",
    fill = "Congruency"
  ) +
  theme_minimal()

# HISTOGRAM OF REACTION TIMES
ggplot(df_filtered, aes(x = rt, fill = congruency)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30, color = "black") +
  scale_fill_manual(values = color_index$congruency_colors) +
  labs(
    title = "Histogram of Reaction Times by Congruency",
    x = "Reaction Time (ms)",
    y = "Count",
    fill = "Congruency"
  ) +
  theme_minimal()

