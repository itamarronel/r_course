# R course for beginners
# Week 4 Assignment by < Itamar > < Ronel >, id < 032702391 >

#### SCRIPT 1 - DATA FRAME AND DESCRIPTIVE STATS  ----


#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Ctrl + L to clear the console.

#### CREATE DATAFRAME ----

# generate vectors
N = 100
subject_id = seq(from = 1, to = N, by = 1) 
gender = factor(sample(c('female', 'male'),
       size = N, replace = TRUE, prob = c(0.5, 0.5)),
       levels = c('female', 'male'))  # מגדר כפקקטור
age = runif(N, 15, 40)
depression = factor(rbinom(N, 1, 0.176), 
       levels = c(0, 1), labels = c("no", "yes"))  # דיכאון כפקקטור
IQ = rnorm(N, mean = 100, sd = 15)  
sleep_hours = rnorm(N, mean = 7, sd = 1.5)  # משתנה שעות שינה

# create data frame
df = data.frame(subject_id, gender, age, depression, IQ, sleep_hours)

# וודא שהתיקיה קיימת
if (!dir.exists("./data")) { dir.create("./data")}

# save 
write.csv(df, file = "./data/df.csv", row.names = FALSE)
save(df, file = "./data/df.rdata")

#### DESCRIPTIVE STATS ----

# base package descriptive data
dim(df)                                      # גודל הטבלה
names(df)                                    # שמות המשתנים
range(df$age)                                # טווח הגיל
mean(df$IQ)                                  # ממוצע IQ
median(ifelse(df$depression == "yes", 100, 0))  # חציון אחוזי הדיכאון 

# dplyr package piping - UNDER 18
library(dplyr)
df |>
  filter(age < 18) |>
  mutate(depression_numeric = ifelse(depression == "yes", 100, 0)) |>  # המרה לערכים מספריים
  group_by(gender) |>                        # קבץ לפי מגדר
  summarize(
    mean_age = mean(age),                    # גיל ממוצע מתחת ל-18 
    mean_iq = mean(IQ),                      # ממוצע מתחת ל-18 IQ
    mean_depression = mean(depression_numeric),  # ממוצע דיכאון מתחת ל-18    
    mean_sleep_hours = mean(sleep_hours)     # ממוצע שעות שינה מתחת ל-18
  )

# dplyr package piping - OVER 18
df |>
  filter(age > 18) |>
  mutate(depression_numeric = ifelse(depression == "yes", 100, 0)) |>  # המרה לערכים מספריים
  group_by(gender) |>                       # קבץ לפי מגדר
  summarize(
    mean_age = mean(age),                   # גיל ממוצע מעל ל-18 
    mean_iq = mean(IQ),                     # ממוצע מעל ל-18 IQ
    mean_depression = mean(depression_numeric),  # ממוצע דיכאון מעל ל-18
    mean_sleep_hours = mean(sleep_hours)    # ממוצע שעות שינה מעל ל-18
  )
