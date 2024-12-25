# R course for beginners
# Week 1
# assignment by < First Name > < Last Name >, id < 123456789 >


#### CREATE DATAFRAME ----

#generate vectors
N = 100
subject_id = seq(from = 1, to = N, by = 1) 
gender = sample(c('female', 'male'), size = N, replace = TRUE, prob = c(0.5, 0.5))
gender = as.factor(gender)
age = runif(N, 15, 40)
depression = rbinom(N, 1, 0.176) 

# create data frame
df = data.frame(subject_id, gender, age, depression)

# save as .csv
write.csv(df, file = "data/df.csv",  row.names = FALSE)

df |>
  select(subject_id, age, depression) |>
  filter(gender == "female") |>
  arrange(age)

sum = df |>
  mutate(depression = depression * 100) |>
  summarise( 
    count = n(),
    min_age = min(age),
    max_age = max(age),
    mean_dep = mean(depression))

df |>
  mutate(depression = depression * 100) |>
  filter(age > 18) |>
  group_by(gender) |>
  summarize (
  count = n(),
  min_age = min(age),
  max_age = max(age),
  mean_depression = mean(depression)
    )
  )
  