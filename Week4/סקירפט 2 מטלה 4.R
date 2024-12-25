#### SCRIPT 2 - DESCRIPTIVE PLOTS  ----

#### PREPARE WORKSPACE & LOAD DATA ----

# Clean the environment
# Ctrl + Shift + F10 to restart the R session (clear workspace and packages)
# Ctrl + L to clear the console.

# Load required libraries
library(dplyr)  
library(ggplot2) 
library(ggdist)  

# Load data 
load("./data/df.rdata")

# VERIFY FACTORS
# Convert gender and depression to factors if not already
df$gender <- factor(df$gender, levels = c("female", "male"))
df$depression <- factor(df$depression, levels = c("no", "yes"))

# Quick check of the data
glimpse(df)   
head(df) 
str(df)
colSums(is.na(df))                        # סופרים ערכים חסרים בכל עמודה
df <- df[complete.cases(df), ]            # שמירת שורות ללא ערכים חסרים בלבד

# Overall statistics
overall_stats <- df |> summarise(
  total_subjects = n(),
  mean_age = mean(age), sd_age = sd(age),
  min_age = min(age), max_age = max(age),
  mean_sleep = mean(sleep_hours), sd_sleep = sd(sleep_hours),
  mean_iq = mean(IQ), sd_iq = sd(IQ),
  min_iq = min(IQ), max_iq = max(IQ),
  depression_rate = mean(depression == "yes") * 100,
  male_count = sum(gender == "male"), female_count = sum(gender == "female")
)

# Statistics by gender
gender_stats <- df |> group_by(gender) |> summarise(
  total_subjects = n(),
  mean_age = mean(age), sd_age = sd(age),
  min_age = min(age), max_age = max(age),
  mean_sleep = mean(sleep_hours), sd_sleep = sd(sleep_hours),
  mean_iq = mean(IQ), sd_iq = sd(IQ),
  min_iq = min(IQ), max_iq = max(IQ),
  depression_rate = mean(depression == "yes") * 100
)


#### UNIFORM VISUALIZATION THEME ----

# Define a consistent theme for all plots
my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12) 
  )

# Define color scheme for plots
colors <- list(
  fill = "#ffe4e1",           # צבע מילוי ורוד בהיר
  outline = "#b16373",        # צבע מתאר ורוד כהה
  male = "#F4C2C2",           # צבע אדום בהיר למשתתפים גברים
  female = "#A4C3D9",         # צבע כחול בהיר למשתתפות נשים
  ci90 = "#d73027",           # צבע אדום עבור רווח סמך של 90%
  ci80 = "#ffc765",           # צבע צהוב עבור רווח סמך של 80%
  ci70 = "#b8f729",           # צבע ירוק עבור רווח סמך של 70%
  mark = '#f2f252',           # סימן צהוב
  depressed_fill = "#87CEFA", # מילוי כחול למדוכאים
  depressed_outline = "#4682B4", # מתאר כחול כהה למדוכאים
  nondepressed_fill = "#98FB98", # מילוי ירוק ללא מדוכאים
  nondepressed_outline = "#3CB371" # מתאר ירוק כהה ללא מדוכאים
)


#### PLOT 1: IQ HISTOGRAM  ----

# Histogram of IQ scores
iq_histogram <- ggplot(data = df, aes(x = IQ)) +               
  geom_histogram(
    binwidth = 5,                                # טווח של כל עמודה
    fill = colors$fill,                          # עיצוב
    color = colors$outline, 
    linewidth = 0.3
  ) +
  scale_x_continuous(breaks = seq(0, max(df$IQ), by = 10)) +  # התאמת ציר איקס
  ggtitle("Histogram of IQ") +                                # כותרות
  xlab("IQ Score") +
  ylab("Frequency") +
  my_theme 


#### PLOT 2: DOT PLOT OF IQ ----

# Dot plot showing IQ distribution
iq_dotplot <- ggplot(data = df, aes(x = IQ)) +
  geom_dotplot(
    binwidth = 5,                              # טווח של כל עמודת נקודות
    dotsize = 0.5,                             # עיצוב הנקודות
    fill = colors$fill,                      
    color = colors$outline,                  
    alpha = 0.8,                              
    stroke = 0.5,                             
    stackdir = "up",                           # כיוון הערימה כלפי מעלה
    stackratio = 1.015                         # התאמה של גובה הערימה
  ) +
  scale_x_continuous(breaks = seq(0, max(df$IQ), by = 5)) +       # התאמת צירים
  scale_y_continuous(breaks = seq(0, 20, by = 1), limits = c(0, 20)) +
  ggtitle("Dot Plot of IQ") +                 # כותרות
  xlab("IQ Score") +                  
  ylab("Frequency") +
  my_theme 


#### PLOT 3: BOX PLOT OF IQ SCORES BY GENDER ----

# Calculate mean IQ for annotation
mean_data <- df |> group_by(gender) |> summarise(mean_IQ = mean(IQ))

# Box plot grouped by gender
iq_gender_boxplot <- ggplot(
  data = df, aes(x = gender, y = IQ, fill = gender)
) +               
  geom_boxplot(
    color = "black",                  # מתאר שחור עבור תיבות הקופסה
    outlier.shape = 21,               # סימון חריגים
    outlier.fill = colors$fill      
  ) +
  stat_summary(                       # סימון ממוצע בנקודה צהובה 
    fun = mean, 
    geom = "point", 
    shape = 23, 
    size = 4, 
    fill = colors$mark        
  ) +
  geom_text(                          # הצגת ממוצעים בתיבות
    data = mean_data, 
    aes(x = gender, y = mean_IQ, label = paste("μ:", round(mean_IQ, 1))),
    color = "black", 
    size = 4, 
    vjust = -2 
  ) +
  ggtitle("Boxplot of IQ by Gender") +                          #כותרות
  xlab("Gender") +
  ylab("IQ Score") +
  scale_fill_manual(
    values = c("male" = colors$male, "female" = colors$female)  # צבעים
  ) +
  my_theme


#### PLOT 4: DENSITY PLOT OF IQ ----

# Density plot for IQ distribution
iq_density <- ggplot(data = df, aes(x = IQ)) +
  geom_density(                                                 # עיצוב 
    fill = colors$fill,        
    color = colors$outline,     
    alpha = 0.8, 
    size = 0.15
  ) +
  scale_x_continuous(breaks = seq(0, max(df$IQ), by = 10)) +  # התאמת ציר איקס   
  ggtitle("Density Plot of IQ") +                             # כותרות  
  xlab("IQ Score") +
  ylab("Density") +
  my_theme                               


#### PLOT 5: IQ DISTREBUTION WITH CI 90, 80, 70 ----

iq_dist_ci <- ggplot(data = df, aes(x = IQ)) +    
  stat_slabinterval(
    aes(                                                # בחירת משתנים
      y = 0,
      x = IQ,
      fill = factor(after_stat(.width)),                  
      color = factor(after_stat(.width))                 
    ),
    .width = c(0.9, 0.8, 0.7),                           # עיצוב עקומה
    slab_fill = colors$fill,
    alpha = 0.7,
    interval_size = 6,
    slab_color = colors$outline,  
    slab_size = 0.3  
  ) +                                                    #  עיצוב רווחי הסמך                               
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
  scale_x_continuous(breaks = seq(40, 150, by = 10)) +      # התאמת ציר איקס
  ggtitle("IQ Distribution with Confidence Intervals") +   # כותרות
  xlab("IQ Score") +
  my_theme +
  theme(
    legend.position = "bottom",                  # מיקום המקרא מתחת לגרף
    axis.title.y = element_blank(),              # הסרת כותרת ציר Y
    axis.text.y = element_blank(),               # הסרת המספרים בציר Y
    axis.ticks.y = element_blank()               # הסרת הסימונים בציר Y
  )


#### PLOT 6: SLAB PLOT FOR IQ DENSITY ----

# Add theoretical density to the data
df <- df |>
  mutate(iq_density_theoretical = dnorm(IQ, mean = 100, sd = 15),
         iq_density = dnorm(IQ, mean = mean(IQ), sd = sd(IQ)))

# Slab plot for theoretical density
iq_density_slab <- ggplot(df, aes(x = IQ)) +
  geom_slab(
    aes(                                        #  הגדרת משתנים
      thickness = iq_density,  
      fill = iq_density_theoretical
    ),
    color = colors$outline,                     # עיצוב קו מתאר      
    alpha = 0.6,
    size = 0.5
  ) +
  scale_fill_gradient(                          # עיצוב צבעים גרדיאנט הצפיפות      
    low = colors$fill, 
    high = colors$ci90,  
    name = "Density"
  ) +                                            
  scale_x_continuous(breaks = seq(50, 150, by = 10)) +    # התאמת ציר איקס
  ggtitle("IQ Density by Normal Distribution") +          # כותרות
  xlab("IQ Score") +
  ylab("") +
  my_theme
iq_density_slab

#### PLOT 7: RAINCLOUD PLOT ----

raincloud_sleep_dep <- ggplot(data = df, aes(x = depression, y = sleep_hours, fill = depression)) +
  stat_halfeye(
    aes(fill = depression, color = depression),
    width = 0.6, 
    justification = -0.3,
    interval_alpha = 0.7,
    point_interval = mean_qi, 
    point_size = 3,
    slab_alpha = 0.6
  ) +
  geom_jitter(
    aes(color = depression), width = 0.2, alpha = 0.6, size = 1.2
  ) +
  scale_fill_manual(
    values = c("no" = colors$nondepressed_fill, 
               "yes" = colors$depressed_fill)
  ) +
  scale_color_manual(
    values = c("no" = colors$nondepressed_outline, 
               "yes" = colors$depressed_outline)
  ) +
  ggtitle("Raincloud Plot of Sleep Hours by Depression Status") +
  xlab("Depression Status") +
  ylab("Sleep Hours") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # הפיכת צירים


#### DISPLAY PLOTS ----

iq_histogram
iq_dotplot
iq_gender_boxplot
iq_density
iq_dist_ci
iq_density_slab
raincloud_sleep_dep
