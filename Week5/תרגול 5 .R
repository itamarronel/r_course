# פונקציה להתקנה וטעינה של חבילות
load_or_install <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)  # התקנה אם החבילה לא קיימת
      library(package, character.only = TRUE)  # טעינה
    } else {
      library(package, character.only = TRUE)  # טעינה אם כבר קיימת
    }
  }
}

# רשימת החבילות הנדרשות
required_packages <- c("dplyr", "ggplot2", "tidyr", "ggpubr")

# הרצת הפונקציה על כל החבילות הנדרשות
load_or_install(required_packages)

# בדיקה שהכול נטען
cat("All required packages are installed and loaded successfully.\n")


set.seed(123)
N <- 200
gender <- factor(sample(c("male", "female"), size = N, replace = TRUE))
group <- factor(sample(c("control", "treatment"), size = N, replace = TRUE))
dependent_variable <- rnorm(N, mean = ifelse(group == "treatment", 70, 50), sd = 10)

df_summary <- df2 |>
  group_by(gender, group) |>
  summarise(mean = mean(dependent_variable), sd = sd(dependent_variable), .groups = "drop")

ggplot(df_summary, aes(x = group, y = mean, color = gender, group = gender)) +
  geom_jitter(
    data = data.frame(gender, group, dependent_variable),  
    aes(x = group, y = dependent_variable, color = gender),  
    size = 1.5, width = 0.4, alpha = 0.3  
  ) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.3), width = 0.2
  ) +
  scale_color_manual(values = c("male" = "red", "female" = "blue")) +
  theme_minimal()

ggerrorplot(
  data = df2, 
  x = "group", 
  y = "mean", 
  desc_stat = "mean_ci", 
  color = "gender", 
  position = position_dodge(0.5)
)

# הצגת הגרף
print(treatment_gender_point)



set.seed(123)  
N <- 100
gender <- factor(sample(c("male", "female"), size = N, replace = TRUE))
group <- factor(sample(c("control", "treatment"), size = N, replace = TRUE))
dependent_variable <- rnorm(N, mean = ifelse(group == "treatment", 70, 50), sd = 10)


df_drill <- data.frame(gender, group, dependent_variable)

model <- lm(dependent_variable ~ gender * group, data = df_drill)

model$coef
summary(model)
print(model)



