# Data Processing Module
# Handles data cleaning, validation, and preprocessing

process_data <- function(raw_data) {
  log_text <- ""
  
  # Log original dimensions
  log_text <- paste0(log_text, "=== DATA CLEANING REPORT ===\n\n")
  log_text <- paste0(log_text, "Original dataset dimensions: ", nrow(raw_data), " rows × ", ncol(raw_data), " columns\n")
  log_text <- paste0(log_text, "Column names: ", paste(colnames(raw_data), collapse = ", "), "\n\n")
  
  grcdata <- raw_data
  
  # Step 1: Remove duplicated rows
  dup_count <- sum(duplicated(grcdata))
  grcdata <- unique(grcdata)
  log_text <- paste0(log_text, "[Step 1] Duplicate Removal:\n")
  log_text <- paste0(log_text, "  - Duplicated rows found: ", dup_count, "\n")
  log_text <- paste0(log_text, "  - Rows after removing duplicates: ", nrow(grcdata), "\n\n")
  
  # Step 2: Handle missing values
  na_count <- sum(is.na(grcdata))
  if (na_count > 0) {
    # Remove rows with NA values
    grcdata <- na.omit(grcdata)
    log_text <- paste0(log_text, "[Step 2] Missing Value Treatment:\n")
    log_text <- paste0(log_text, "  - NA values found: ", na_count, "\n")
    log_text <- paste0(log_text, "  - Rows with NA removed\n")
    log_text <- paste0(log_text, "  - Rows after NA removal: ", nrow(grcdata), "\n\n")
  } else {
    log_text <- paste0(log_text, "[Step 2] Missing Value Treatment:\n")
    log_text <- paste0(log_text, "  - No missing values found\n\n")
  }
  
  # Step 3: Outlier detection and removal using IQR method
  if ("total" %in% colnames(grcdata)) {
    # Calculate IQR for total spending
    Q1 <- quantile(grcdata$total, 0.25, na.rm = TRUE)
    Q3 <- quantile(grcdata$total, 0.75, na.rm = TRUE)
    IQR_val <- IQR(grcdata$total, na.rm = TRUE)
    
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    outliers <- sum(grcdata$total < lower_bound | grcdata$total > upper_bound)
    
    # Subset data to remove outliers
    grcdata <- subset(grcdata, grcdata$total >= lower_bound & grcdata$total <= upper_bound)
    
    log_text <- paste0(log_text, "[Step 3] Outlier Detection & Removal (IQR Method):\n")
    log_text <- paste0(log_text, "  - Q1 (25th percentile): ", round(Q1, 2), "\n")
    log_text <- paste0(log_text, "  - Q3 (75th percentile): ", round(Q3, 2), "\n")
    log_text <- paste0(log_text, "  - IQR: ", round(IQR_val, 2), "\n")
    log_text <- paste0(log_text, "  - Lower bound: ", round(lower_bound, 2), "\n")
    log_text <- paste0(log_text, "  - Upper bound: ", round(upper_bound, 2), "\n")
    log_text <- paste0(log_text, "  - Outliers detected: ", outliers, "\n")
    log_text <- paste0(log_text, "  - Rows after outlier removal: ", nrow(grcdata), "\n\n")
  }
  
  # Final summary
  log_text <- paste0(log_text, "=== CLEANING SUMMARY ===\n")
  log_text <- paste0(log_text, "Final dataset: ", nrow(grcdata), " rows × ", ncol(grcdata), " columns\n")
  log_text <- paste0(log_text, "Data ready for analysis!")
  
  return(list(data = grcdata, log = log_text))
}