#### SCRIPT 2 - DESCRIPTIVE STATS & PLOTS  ----

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

# a. basic summery
summary(df2)
df2 |>
  reframe(age_range = range(age), age_mean = mean(age), age_sd = sd(age))

#b. Factorial Design
df2_summary <- df2 |>
  group_by(group, treatment) |>
  reframe(mean = mean(memory),se = sd(memory) / sqrt(n()), .groups = "drop")

# c. plot manually with ggplot2
tms_ggplot = ggplot(df2_summary, aes(x = group, y = mean, color = treatment, group = treatment)) +
  geom_jitter(data = df2, 
    aes(x = group, y = memory, color = treatment),  
    size = 1.5, width = 0.4, alpha = 0.3  
  ) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.3), width = 0.2
  ) +
  scale_color_manual(
    values = c("placebo" = "#7d7d7d", "TMS" = "#2020f3"), 
    labels = c("Placebo (Control)", "TMS (Treatment)") 
  ) +
  theme_minimal() +
  labs(title = "Memory Scores by Group and Treatment",
       x = "Group", y = "Mean Memory Score", color = "Treatment Group")

# d. plot with ggpubr
tms_ggpubr = ggerrorplot(data = df2,  x = "group",  y = "memory",
              desc_stat = "mean_se", color = "treatment", 
              add = "jitter", add.params = list(alpha = 0.35)) +
  theme_minimal()
  
# display plots
tms_ggpubr
tms_ggplot
  
  