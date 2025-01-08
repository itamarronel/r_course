# בניית הפונקציה
sum_two_numbers <- function(a, b) {
  result <- a + b  # פעולה: חיבור של שני המספרים
  return(result)   # פלט: החזרת התוצאה
}

# שימוש בפונקציה
sum_two_numbers(3, 7)  # הקלט: 3 ו-7


# בניית הפונקציה
multiply_two_numbers <- function(a, b) {
  result <- a * b  # פעולה: הכפלת שני המספרים
  return(result)   # פלט: החזרת התוצאה
}

# שימוש בפונקציה
multiply_two_numbers(4, 5)  # הקלט: 4 ו-5

source("/Users/itamar/Documents/GitHub/r_course")  # נתיב יחסי
source("functions.R")



# בניית הפונקציה
conditional_operation <- function(a, b) {
  if (a > 5 & b > 5) {  # תנאי: שני המספרים גדולים מ-5
    result <- a * b     # פעולה: הכפלה
  } else {
    result <- a + b     # פעולה: חיבור
  }
  return(result)         # פלט: החזרת התוצאה
}

# שימוש בפונקציה
conditional_operation(6, 7)  # שני המספרים גדולים מ-5 -> תוצאה: 42 (כפל)
conditional_operation(4, 6)  # אחד קטן או שווה ל-5 -> תוצאה: 10 (חיבור)



process_input <- function(input) {
  if (is.character(input)) {  # בודקים אם הקלט הוא מחרוזת
    print(paste("זה טקסט שהוזן:", input))  # מדפיסים את הטקסט
  } else if (is.numeric(input)) {  # בודקים אם הקלט הוא מספרים
    print(paste("זה מספר או וקטור מספרי, והסכום הוא:", sum(input)))
  } else {
    print("הקלט אינו מחרוזת או מספר")
  }
}

process_input("שלום!")  # פלט: "זה טקסט שהוזן: שלום!"
process_input(c(1, 2, 3))  # פלט: "זה מספר או וקטור מספרי, והסכום הוא: 6"
process_input(TRUE)  # פלט: "הקלט אינו מחרוזת או מספר"

multiply_even_odd <- function(numbers) {
  # סינון של מספרים שונים
  non_zero_numbers <- numbers[numbers != 0]  # התעלמות מאפסים
  even_numbers <- non_zero_numbers[non_zero_numbers %% 2 == 0]  # זוגיים
  odd_numbers <- non_zero_numbers[non_zero_numbers %% 2 != 0]  # אי-זוגיים
  
  # חישוב מכפלות
  even_product <- ifelse(length(even_numbers) > 0, prod(even_numbers), 1)  # אם אין זוגיים, החזר 1
  odd_product <- ifelse(length(odd_numbers) > 0, prod(odd_numbers), 1)    # אם אין אי-זוגיים, החזר 1
  
  # הצגת התוצאות
  cat("מכפלת הזוגיים:", even_product, "\n")
  cat("מכפלת האי-זוגיים:", odd_product, "\n")
}


