################################################################################
## 제작일: 2024-07-29
## 인  자: dataframe, cut_off
## 반환값: NA 비율 기준에 따라 NA가 많은 열을 제거한 데이터프레임
## 참  고: 제거되는 열이름과 NA수를 출력
################################################################################
my_eliminate_NA_columns <- function(df, cut_off) {
  
  # Calculate the threshold for the number of NAs
  NA_threshold <- nrow(df) * (cut_off / 100)
  # Initialize vectors to store column names and NA counts
  columns_with_na <- c()
  na_counts <- c()
  
  # Determine which columns to retain
  columns_to_retain <- sapply(names(df), function(column_name) {
    # Count the number of NAs in the column
    na_count <- sum(is.na(df[[column_name]]))
    
    # Store the column name and NA count if the number of NAs is greater than the threshold
    if (na_count > NA_threshold) {
      columns_with_na <<- c(columns_with_na, column_name)
      na_counts <<- c(na_counts, na_count)
      return(FALSE)  # Do not retain this column
    }
    
    return(TRUE)  # Retain this column
  })
  
  # Subset the data to keep only the desired columns
  cleaned_df <- df[, columns_to_retain, drop = FALSE]
  
  # Print columns with NAs and their counts that are above the threshold
  if (length(columns_with_na) > 0) {
    cat("Columns with NAs above the threshold:\n")
    for (i in seq_along(columns_with_na)) {
      cat(columns_with_na[i], ": ", na_counts[i], " NAs\n", sep = "")
    }
  } else {
    cat("No columns with NAs above the threshold found.\n")
  }
  
  # Return the cleaned data
  return(cleaned_df)
}


################################################################################
## 함수명: my_NA_row_clean 
## 제작일: 2024-07-17
## 인  자: 데이터프레임
## 반환값: NA가 포함된 열을 제거한 데이터프레임
################################################################################
my_eliminate_NA_rows <- function(data) {
  # Identify columns with NAs and count the number of NAs in each column
  na_info <- sapply(data, function(column) {
    sum(is.na(column))
  })
  
  # Get column names with NAs
  columns_with_na <- names(na_info)[na_info > 0]
  
  # Print column names and number of NAs
  if (length(columns_with_na) > 0) {
    cat("Columns with NAs:\n")
    for (column in columns_with_na) {
      cat(column, ": ", na_info[column], " NAs\n", sep = "")
    }
  } else {
    cat("No columns with NAs found.\n")
  }
  
  # Remove rows with NAs
  cleaned_data <- na.omit(data)
  
  return(cleaned_data)
}

################################################################################
## 제작일: 2024-07-29
## 인  자: 데이터프레임
## 출  력: 연속적변수에 대한 히스토그램수
################################################################################
my_histograms <- function(dataframe) {

  numeric_cols <- sapply(dataframe, is.numeric)

  for (col_name in names(dataframe)[numeric_cols]) {
    print(col_name)
    p<-ggplot(dataframe, aes_string(x = col_name)) +
      geom_histogram() +
      labs(title = paste("Histogram of", col_name),
           x = col_name,
           y = "Frequency") +
      theme_minimal()
    print(p)
      }
}


