# יצירת נתונים לדוגמה
set.seed(123)
x <- rnorm(100)
p <- 1 / (1 + exp(- (1 + 2*x)))  # פונקציית לוגיט לחישוב הסתברות
y <- rbinom(100, size = 1, prob = p)  # יצירת משתנה בינארי

reg_log <- glm(y ~ x,  data = df, family = binomial())

# התאמת רגרסיה לוגיסטית
mod_logistic <- glm(y ~ x, family = binomial())

# הצגת סיכום המודל
summary(mod_logistic)

# הצגת תקציר המודל עם summary()
summary(mod_logistic)

# הצגת מקדמים בלבד עם coef()
coef(mod_logistic)

# התקנת החבילה אם אין לך
install.packages("rstanarm")

# טעינת החבילה
library(rstanarm)

# התאמת רגרסיה לוגיסטית בייסיאנית
mod_bayes <- stan_glm(y ~ x, family = binomial(), data = my_data, prior = normal(0, 10))

# הצגת תוצאות
summary(mod_bayes)


#### מודל פשוט לדוגמה

# יצירת נתוני דוגמה פשוטים (6 תצפיות בלבד)
x <- c(2, 3, 5, 7, 9, 11)  # משתנה מסביר
y <- c(0, 0, 1, 1, 1, 1)    # משתנה תלוי בינארי (0/1)

# התאמת מודל רגרסיה לוגיסטית
reg_log <- glm(y ~ x, family = binomial())

# הצגת סיכום המודל
summary(reg_log)

# חיזוי הסתברויות ל-y באמצעות המודל
pred_probs <- predict(reg_log, type = "response")

# הצגת ההסתברויות שחושב המודל
print(pred_probs)

plot(x, pred_probs, ylim = c(0,1), pch = 16, col = "blue", main = "Predicted Probabilities")

# טעינת חבילת pROC
install.packages("pROC")
library(pROC)

# חישוב ה-ROC וה-AUC
roc_curve <- roc(y, pred_probs)  # יצירת עקומת ROC
auc_value <- auc(roc_curve)  # חישוב AUC

# הצגת הערך של AUC
print(auc_value)

# ציור עקומת ה-ROC
plot(roc_curve, col = "blue", main = "ROC Curve", lwd = 2)

# הוספת קו אלכסוני (מייצג ניחוש רנדומלי)
abline(a = 1, b = -1, lty = 2, col = "red")

# חישוב מדד Youden ובחירת הסף האופטימלי
best_threshold <- coords(roc_curve, "best", ret = "threshold")
print(best_threshold)

best_accuracy_threshold <- coords(roc_curve, "best", ret = c("threshold", "accuracy"))
print(best_accuracy_threshold)

