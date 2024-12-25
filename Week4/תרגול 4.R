# הגדרת מספר התצפיות
N <- 30  

# יצירת משתנה sleep_duration שנדגם מהתפלגות נורמלית
sleep_duration <- rnorm(N, mean = 6, sd = 4)

# ביצוע מבחן t חד-זנבי לבדוק אם ממוצע המדגם גדול מ-4
t_test_sleep <- t.test(sleep_duration, alternative = "greater", mu = 4, conf.level = 0.99)

# שליפת ערכים מתוך תוצאת המבחן
t_sleep <- t_test_sleep$statistic    
p_sleep <- t_test_sleep$p.value        

# הדפסת הערכים
print(t_sleep)
print(p_sleep)

data <- data.frame(
  y = c(rnorm(100, mean = 5, sd = 1), rnorm(100, mean = 7, sd = 1.5)),
  group = rep(c("Group A", "Group B"), each = 100))
t_test_result <- t.test(y ~ group, data = data, altenative = "less")

# הדפסת תוצאת המבחן
print(t_test_result)