#### SCRIPT 2 - DESCRIPTIVE STATS & PLOTS  ----

#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Cmd + Shift + F10 (for mac)
# Ctrl + L to clear the console.

# Load required packages
required_packages <- c("dplyr", "ggplot2", "ggpubr")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")

# Load data
load("./data/df.rdata")

#### DEFINE COLOR INDEX ----
color_palette <- list(
  points = "#1f77b4", # Blue
  reg_line = "#d62728", # Red
  conf_band = "#2ca02c" # Green
)

#### 1. DESCRIPTIVE STATISTICS ----

cat("\n--- Descriptive Statistics ---\n")
age_summary <- df %>%
  summarise(
    Range = max(Age) - min(Age),
    Mean = mean(Age),
    SD = sd(Age)
  )
print(age_summary)

gender_counts <- df %>%
  group_by(Gender) %>%
  summarise(Count = n())
print(gender_counts)

#### 2. VISUALIZATIONS ----

# (A) BASE R SCATTER PLOT ----
cat("\n--- Base R Scatter Plot ---\n")
plot(df$Stress, df$Satisfaction,
     main = "BASE R SCATTER PLOT",
     xlab = "Stress",
     ylab = "Satisfaction",
     pch = 19,
     col = color_palette$points)
abline(lm(Satisfaction ~ Stress, data = df), col = color_palette$reg_line)

# (B) GGPLOT2 SCATTER PLOT ----
cat("\n--- ggplot2 Scatter Plot ---\n")
p1 <- ggplot(data = df, aes(x = Stress, y = Satisfaction)) +
  geom_point(color = color_palette$points, alpha = 0.6, shape = 8, size = 3) +
  geom_smooth(method = "lm",
              color = color_palette$reg_line,
              fill = color_palette$conf_band,
              se = TRUE) +
  labs(title = "GGPLOT2 SCATTER PLOT",
       x = "Stress",
       y = "Satisfaction")

# (C) GGSCATTER SCATTER PLOT ----
cat("\n--- ggscatter Scatter Plot ---\n")
p2 <- ggscatter(
  data = df,
  x = "Stress",
  y = "Satisfaction",
  color = color_palette$points,
  size = 2,
  alpha = 0.6,
  add = "reg.line",
  conf.int = TRUE,
  add.params = list(color = color_palette$reg_line, fill = color_palette$conf_band),
  cor.coef = TRUE,
  cor.method = "pearson"
) + 
  ggtitle("GGSCATTER SCATTER PLOT") +
  xlab("Stress") +
  ylab("Satisfaction")

#### PRINT ALL GRAPHS ----
print(p1)
print(p2)
