# R Course: Functions Script
# <Week 9> Assignment by <Itamar Ronel>, ID <032702391>
# Script: functions.R

#### FUNCTION: GENERATE DESCRIPTIVE STATISTICS ----
# Function to compute descriptive statistics for a data frame
# (Include the `generate_statistics` function here)

# Function to generate descriptive statistics
generate_statistics <- function(data, subject_start = NULL, subject_end = NULL) {
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Filter data based on subject range
  if (!is.null(subject_start) & !is.null(subject_end)) {
    data <- data[data$subject_id >= subject_start & data$subject_id <= subject_end, ]
  }
  
  # Check if the data has fewer than 10 rows AFTER filtering
  if (nrow(data) < 10) {
    stop("Data is too short")
  }
  
  # Create an empty list to store results
  results <- list()
  
  # Loop through each column in the data frame
  for (col_name in colnames(data)) {
    col_data <- data[[col_name]]  # Extract column data
    
    if (class(col_data) %in% c("numeric", "integer")) {  # Continuous variables
      stats <- data.frame(
        variable = col_name,
        min = min(col_data, na.rm = TRUE),
        max = max(col_data, na.rm = TRUE),
        mean = mean(col_data, na.rm = TRUE),
        summary = NA  # Placeholder for categorical variables
      )
    } else if (class(col_data) %in% c("factor", "character")) {  # Categorical variables
      levels_table <- as.data.frame(table(col_data))
      colnames(levels_table) <- c("level", "frequency")
      stats <- data.frame(
        variable = col_name,
        min = NA,
        max = NA,
        mean = NA,
        summary = paste(levels_table$level, levels_table$frequency, sep = ": ", collapse = ", ")
      )
    } else {  # Unsupported data types
      stats <- data.frame(
        variable = col_name,
        min = NA,
        max = NA,
        mean = NA,
        summary = "Unsupported data type"
      )
    }
    
    # Add the stats for the column to the results list
    results[[col_name]] <- stats
  }
  
  # Combine all results into a single data frame
  analysis_df <- do.call(rbind, results)
  rownames(analysis_df) <- NULL
  
  return(analysis_df)
}
