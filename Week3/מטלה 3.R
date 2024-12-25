# R course for beginners
# Week 3
# assignment by < Itamar > < Ronel >, id < 032702391 >

#### PREPER WORKSPACE & DATAFRAME ----

# Clean Workspace
rm(list = ls()) 

# Load packeges 
library(dplyr)  
library(ggplot2) 
library(ggdist)  

# Load data frame
df <- read.csv("df.csv", stringsAsFactors = FALSE)

# quick check of data frame
glimpse(df)
head(df) 
str(df)
colSums(is.na(df))

#### DESCRIPTIVE STATISTICS ----

# use dplyr - GENERAL
df |>
  summarise(
    mean_age = mean(age),
    sd_age = sd(age),
    min_age = min(age), max_age = max(age),
    mean_iq = mean(IQ), 
    sd_iq = sd(IQ),
    min_iq = min(IQ), max_iq = max(IQ)
  )

# use dplyr - BY GENDER
df |>
  group_by(gender) |>                    
  reframe(
    mean_age = mean(age),
    sd_age = sd(age),
    min_age = min(age), max_age = max(age),
    mean_iq = mean(IQ), 
    sd_iq = sd(IQ),
    min_iq = min(IQ), max_iq = max(IQ)
  )


# תמה אחידה לכל הגרפים
my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12) )
colors <- list(
    fill = "#ffe4e1",
    outline = "#b16373",
    male = "#F4C2C2",
    female = "#A4C3D9",
    ci90 = "#d73027",
    ci80 = "#ffc765",
    ci70 = "#b8f729"
    )

# 1. גרף היסטוגרמה (Histogram)
iq_histogram <- ggplot(data = df, aes(x = IQ)) +               
  geom_histogram(binwidth = 5,
     fill = colors$fill, color = colors$outline, size = 0.3) +        #צבעים ועיצוב
  scale_x_continuous(breaks = seq(0, max(df$IQ), by = 10)) +         # x axis scale
  ggtitle("Histogram of IQ") +                                       # כותרות
     xlab("IQ Score") +
     ylab("Frequency") +
  my_theme                                    

# 2. גרף נקודות (Dot Plot)
iq_dotplot <- ggplot(data = df, aes(x = IQ)) +
  geom_dotplot(
    binwidth = 5,                              # רוחב הבין (bin width)
    dotsize = 0.5,                             # גודל הנקודות
    fill = colors$fill,                        # צבע מילוי נקודות
    color = colors$outline,                    # צבע מתאר נקודותֿ
    alpha = 0.8,                               # שקיפות
    stroke = 0.5,                              # עובי קו המתאר של הנקודות
    stackdir = "up",                           # ערימת הנקודות כלפי מעלה
    stackratio = 1.015,                        # יחס ערימה: כל נקודה מייצגת נבדק אחד
     ) +
  scale_x_continuous(                          # x axis
    breaks = seq(0, max(df$IQ), by = 5)        # סימונים בקפיצות של 5
    ) + 
  scale_y_continuous(                          # y axis
    breaks = seq(0, 20, by = 1),               # קפיצות של 1 בציר
    limits = c(0, 20),                         # טווח הציר
  ) +
  ggtitle("Dot Plot of IQ") +                  # כותרות
  xlab("IQ Score") +                  
  ylab("Frequency") +
  my_theme                                    

# 3. גרף קופסה (Boxplot) 
# הכנה: יצירת טבלה עם הממוצעים לפי מגדר
mean_data <- df |>
  group_by(gender) |>
  summarise(mean_IQ = mean(IQ))               # חישוב ממוצע IQ לכל מגדר

# יצירת גרף Boxplot
iq_gender_boxplot <- ggplot(data = df,
  aes(x = gender, y = IQ, fill = gender)) +               
  geom_boxplot(                               # עיצוב קופסאות              
    color = "black",
    outlier.shape = 21,
    outlier.fill = "white",
    outlier.color = "black"
    ) +
  stat_summary(                               #.  נקודה לסימון ממוצע
    fun = mean, 
    geom = "point", 
    shape = 23, 
    size = 4, 
    fill = "#ffc765"
    ) +
  scale_y_continuous(                                           #. y axis scale
    breaks = seq(0, max(df$IQ), by = 10)  
  ) +
  geom_text(                                                    #. כיתוב ממוצעים בקופסאות
    data = mean_data, 
    aes(x = gender, y = mean_IQ, label = paste("μ:", round(mean_IQ, 1))),  
    color = "black", 
    size = 4, 
    vjust = -2 
  ) +
  ggtitle("Boxplot of IQ by Gender") +                           # כותרות וכיתובים
  ylab("IQ Score") +
  xlab("Gender") +
  scale_x_discrete(
    labels = c("male" = "Male", "female" = "Female")
  ) +
  scale_fill_manual(                                             # צבעים                                       
    values = c("male" = colors$male, "female" = colors$female),
    labels = c("male" = "Male", "female" = "Female")
  ) +
  my_theme

# 4. גרף צפיפות (Density Plot)
iq_density <- ggplot(data = df, aes(x = IQ)) +
  geom_density(fill = colors$fill, color = colors$outline, alpha = 0.8, size = 0.15) +          
  scale_x_continuous(breaks = seq(0, max(df$IQ), by = 10)) +          
  ggtitle("Density Plot of IQ") +            
  xlab("IQ Score") +
  ylab("Density") +
  my_theme                               

# הצגת גרפים
iq_histogram
iq_dotplot
iq_gender_boxplot
iq_density

iq_slabinterval <- ggplot(data = df, aes(x = IQ)) +
  stat_slabinterval(
    aes(
      y = 0,
      x = IQ,
      fill = factor(after_stat(.width)),                  
      color = factor(after_stat(.width))                 
    ),
    .width = c(0.9, 0.8, 0.7),
    slab_fill = colors$fill,
    alpha = 0.7,
    interval_size = 6,
    slab_color = "#b16373"  # קו מתאר דק
  ) +
  scale_fill_manual(
    values = c("0.9" = colors$ci90, "0.8" = colors$ci80, "0.7" = colors$ci70), 
    labels = c("90% CI", "80% CI", "70% CI"),                         
    name = "Confidence Interval"
  ) +
  scale_color_manual(
    values = c("0.9" = colors$ci90, "0.8" = colors$ci80, "0.7" = colors$ci70),
    labels = c("90% CI", "80% CI", "70% CI"),
    name = "Confidence Interval"
  ) +
  ggtitle("IQ Distribution with Confidence Intervals") +
  xlab("IQ Score") +
  ylab("Density") +
  my_theme +
  theme(legend.position = "bottom")  # מיקום המקרא מתחת לגרף


# הוספת עמודת צפיפות תיאורטית
df <- df %>%
  mutate(
    iq_density_theoretical = dnorm(IQ, mean = 100, sd = 15)  # צפיפות בהתפלגות נורמלית
  )
# גרף שמציג את הדגימות שלך כסלאב על בסיס הצפיפות התיאורטית
ggplot(df, aes(x = IQ)) +
  # הצגת הסלאב עבור הדגימות
  geom_slab(
    aes(
      y = 0,  # יושב בגובה 0
      thickness = iq_density_theoretical,  # עובי הסלאב משקף את הצפיפות התיאורטית
      fill = iq_density_theoretical        # מילוי לפי הצפיפות
    ),
    color = "#1f78b4",  # צבע מתאר
    size = 0.5,
    alpha = 0.6         # שקיפות
  ) +
  scale_x_continuous(breaks = seq(50, 150, by = 10)) +  # טווח וציון ערכים
  scale_fill_continuous(name = "Density") +             # מקרא לצבעים
  ggtitle("Density of IQ Based on Theoretical Normal Distribution") +
  xlab("IQ Score") +
  ylab("") +
  theme_minimal(base_size = 14)

