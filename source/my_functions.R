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
## 제작일: 2024-08-11
## 인  자: 데이터프레임
## 출  력: 각 컬럼별 히스토그램 또는 막대그래프
################################################################################
my_plot_columns <- function(df) {
  
  results <- list()
  
  col_names <- names(df)
  for (col in col_names) {
    if (is.numeric(df[[col]])) {
      col_mean <- mean(df[[col]], na.rm = TRUE)
      col_sd <- sd(df[[col]], na.rm = TRUE)
      
      lower_bound <- col_mean - 3 * col_sd
      upper_bound <- col_mean + 3 * col_sd
      
      outliers <- df[[col]][df[[col]] < lower_bound | df[[col]] > upper_bound]
      
      # If there are outliers, store the result
      if (length(outliers) > 0) {
        outliers <- sort(outliers)  # Sort outliers in increasing order
        
        # Store results in the list
        results[[col]] <- list(
          mean = col_mean,
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          outliers = outliers
        )
        
        # Plot histogram with custom coloring
        breaks <- pretty(df[[col]], n = 30)
        bins <- cut(df[[col]], breaks = breaks, include.lowest = TRUE)
        bin_colors <- ifelse(as.numeric(breaks[-length(breaks)]) < lower_bound |
                               as.numeric(breaks[-1]) > upper_bound, "red", "gray")
        
        hist(df[[col]], main = col, xlab = col, col = bin_colors, breaks = breaks)
        
        abline(v = col_mean, col = "red", lwd = 2, lty = 2)
        abline(v = lower_bound, col = "red", lwd = 2, lty = 2)
        abline(v = upper_bound, col = "red", lwd = 2, lty = 2)
        text(x = col_mean, y = par("usr")[4] * 0.9, 
             labels = paste("Mean:", round(col_mean, 2)), 
             col = "red", cex = 0.8, pos = 4)
        shapiro_result <- shapiro.test(df[[col]])
        mtext(paste("Shapiro-Wilk p-value:", format(shapiro_result$p.value, digits = 4)), 
              side = 3, line = -1, adj = 0.95, cex = 0.8, col = "blue")
      }
    } else {
      barplot(table(df[[col]]), main = col, xlab = col)
    }
  }
  
  return(results)
}

my_typecasting <- function(df) {
  # data.frame의 각 열에 대해 반복
  df[] <- lapply(df, function(col) {
    if (is.numeric(col) && all(col %in% c(0, 1))) {
      # 0과 1로만 구성된 열을 factor로 변환
      return(factor(col, levels = c(0, 1), labels = c("No", "Yes")))
    } else {
      # 나머지 열은 그대로 유지
      return(col)
    }
  })
  return(df)
}
