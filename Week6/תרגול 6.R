
#### PREPARE WORKSPACE & LOAD DATA ----
# Clean the environment
# Cmd + Shift + F10 - for mac
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Ctrl + L to clear the console.

#load data and packeges
load_or_install <- function(packages) 
{for (pkg in packages) 
{if (!require(pkg, character.only = TRUE)) 
{install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)}}}

required_packages <- c("dplyr", "ggplot2", "tidyr", "ggpubr", "effectsize")
load_or_install(required_packages)
cat("All required packages are installed and loaded successfully.\n")
load("./data/df2.rdata")


# יצירת המשתנים
x = rnorm(100, mean = 100, sd = 25)  # משתנה x
y = rnorm(100, mean = 100 + 3 * x, sd = 25)  # משתנה y עם קשר מוגדר
model = lm(y ~ x)
summary(model)
correlation = cor(x, y, method = "pearson")
print(correlation)

# Preview the data
print(df)
ggscatter(
  data = df, 
  x = "Satisfaction", 
  y = "Stress",
  color = "blue", 
  size = 2, 
  alpha = 0.6, 
  add = "reg.line",                
  conf.int = TRUE,                 
  add.params = list(color = "red"),
  cor.coef = TRUE,                # הצגת מקדם המתאם
  cor.method = "pearson"          # שיטת החישוב של המתאם
)


# יצירת המשתנים
x = rnorm(100, mean = 100, sd = 25)
y = rnorm(100, mean = 100 + 3 * x, sd = 25)

# תקנון המשתנים
x_scaled = scale(x)  # x מתוקנן
y_scaled = scale(y)  # y מתוקנן

# חישוב מודל רגרסיה עם המשתנים המתוקננים
model_scaled = lm(y_scaled ~ x_scaled)

correlation = cor(x, y, method = "pearson")
print(correlation)

# סיכום המודל המתוקנן
summary(model_scaled)




x = rnorm(1000, 100, 50)
z = scale(x)
z = as.vector(z)
print(z)

x = rnorm(1000, 100, 50)
y = rnorm(1000, 50, 100)
plot(x, y) 
abline(lm(y ~ x), col = "red")

x = rnorm(100, mean = 50, sd = 10)  # משתנה רציף ראשון
y = rnorm(100, mean = 30, sd = 5)   # משתנה רציף שני
plot(x, y)                          # יצירת scatterplot

# התקנת וטעינת ggpubr אם לא מותקנת
if (!require("ggpubr")) install.packages("ggpubr")
library(ggpubr)

# יצירת המשתנים
x = rnorm(100, mean = 100, sd = 20)  # משתנה x
y = rnorm(100, mean = x, sd = 20)    # משתנה y עם תלות ב-x

# Scatterplot עם קו רגרסיה
ggscatter(
  data = data.frame(x, y), 
  x = "x", 
  y = "y",
  color = "blue", 
  size = 2, 
  alpha = 0.6, 
  add = "reg.line",                # הוספת קו רגרסיה
  conf.int = TRUE,                 # פסי אמון
  add.params = list(color = "red") # קו רגרסיה בצבע אדום
)
או
ggscatter(
  data = data.frame(x, y), 
  x = "x", 
  y = "y",
  color = "blue", 
  size = 2, 
  alpha = 0.6, 
  add = "reg.line",                
  conf.int = TRUE,                 
  add.params = list(color = "red"),
  cor.coef = TRUE,                # הצגת מקדם המתאם
  cor.method = "pearson"          # שיטת החישוב של המתאם
)



# שרטוט scatterplot בסיסי
ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
       geom_point()

x = rnorm(1000, 100, 50) 
y = rnorm(1000, x, 50)   
# scatterplot
ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.4, size = 0.4) + 
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  xlim(-100, 300) +                            
  ylim(-110, 350) +                            
  labs(title = "Scatterplot of X and Y", x = "Variable X", y = "Variable Y") +
  theme_minimal()  
