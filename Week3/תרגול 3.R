##### תרגול שני

#### CREATE DATAFRAME ----

# generate vectors
N = 100
subject_id = seq(from = 1, to = N, by = 1) 
gender = sample(c('female', 'male'), size = N, replace = TRUE, prob = c(0.5, 0.5))
gender = as.factor(gender)
age = runif(N, 15, 40)
depression = rbinom(N, 1, 0.176) 
IQ = rnorm(N, mean = 100, sd = 15)  

# create data frame
df = data.frame(subject_id, gender, age, depression, IQ)

# save as .csv
write.csv(df, file = "df.csv", row.names = FALSE)

library(ggplot2)
library(ggdist)

mean_iq <- mean(df$IQ)
sd_iq <- sd(df$IQ)

iq_density <- dnorm(df$IQ, mean = mean_iq, sd = sd_iq)  # חישוב צפיפות והסרת NA
iq_density_df <- data.frame(
  x = df$IQ,  # ערכי ה-IQ
  y = iq_density   # צפיפות
)
# save as .csv
write.csv(iq_density_df, file = "iq_density_df.csv", row.names = FALSE)

ggplot(data = iq_density_df, aes(x = x, y = y)) +
  geom_slabinterval(
    aes(fill = "blue"),              # מילוי לפי רמות צפיפות
    color = "#f8a8ae",          # צבע קו המתאר
    alpha = 0.6,                # שקיפות כללית
    thickness = 0.5             # עובי המשטח
  ) +
  xlab("IQ") +
  ylab("Density") +
  ggtitle("Probability Density Plot: IQ") +
  theme_minimal()


x = seq(0, 200, by = 0.1)
x_density = dnorm(x, mean = 100, sd = 50, log = FALSE)

x_density_df = data.frame(
  x = x,  # ערכי ה-IQ
  y = x_density   # צפיפות
)
# save as .csv
write.csv(x_density_df, file = "x_density_df.csv", row.names = FALSE)

library(ggplot2)

# גרף הצפיפות
ggplot(data = x_density_df, aes(x = x, y = y)) +
  geom_line(fill = "blue", color = "blue", linewidth = 1) +  # קו צפיפות
  xlab("IQ") +
  ylab("Density") +
  ggtitle("Probability Density Plot: IQ") +
  theme_minimal()



