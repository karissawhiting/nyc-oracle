
library(RSocrata)
library(dplyr)
library(lubridate)
library(forecast)
library(tibbletime)
library(forecast)


find_time_series_cols <- function(df) {
  # Date/time columns
  date_cols <- names(df)[sapply(df, function(x) any(class(x) %in% c("POSIXct","Date")))]
  if (length(date_cols) == 0) {
    date_cols <- names(df)[grepl("date|time|created", names(df), ignore.case = TRUE)]
  }
  
  # Numeric columns
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- num_cols[!str_detect(num_cols, "_id|zip|latitude|longitude")]
  list(dates = date_cols, nums = num_cols)
}



seasonality_score <- function(ts_data, frequency = 365) {
  tryCatch({
    ts_obj <- ts(ts_data, frequency = frequency)
    decomp <- stl(ts_obj, s.window = "periodic")
    var_seasonal <- var(decomp$time.series[, "seasonal"])
    var_total <- var(ts_obj)
    score <- var_seasonal / var_total
    return(score)
  }, error = function(e) NA)
}


acf_score <- function(ts_data) {
  acf_values <- acf(ts_data, plot = FALSE)$acf[-1]
  score <- max(abs(acf_values))
  return(score)
}




seasonality_mstl <- function(x, max_lag = 400, top_n = 3) {
  
  # --- 1. Detect seasonality via ACF and FFT ---
  ac <- acf(x, lag.max = max_lag, plot = FALSE)$acf[-1]
  ac_peaks <- which(diff(sign(diff(ac))) == -2) + 1
  
  spec <- spectrum(x, plot = FALSE)
  periods_fft <- round(1 / spec$freq)
  
  # candidates
  cand_acf <- ac_peaks[order(-ac[ac_peaks])][1:min(top_n, length(ac_peaks))]
  cand_fft <- periods_fft[order(-spec$spec)][1:min(top_n, length(periods_fft))]
  
  # combine and clean
  periods <- sort(unique(round(c(cand_acf, cand_fft))))
  periods <- periods[periods >= 2 & periods <= max_lag]
  
  if (length(periods) == 0) {
    message("No seasonality detected")
    return(NULL)
  }
  
  # --- 2. Run MSTL with detected periods ---
  ts_obj <- ts(x, frequency = 365.25)  # base frequency for daily data
  decomp <- msts(ts_obj, seasonal.periods = periods)
  
  # --- 3. Compute seasonality strength for each component ---
  seasonal_cols <- grep("Seasonal", colnames(decomp), value = TRUE)
  total_var <- var(x)
  
  strengths <- sapply(seasonal_cols, function(col) {
    var(decomp[, col]) / total_var
  })
  
  result <- data.frame(
    period = periods,
    component = seasonal_cols,
    strength = strengths
  )
  
  return(result)
}



# Function to Analyze Seasonality  ---------------------------------------

process_cycles <- function(dataset_id) {
  
  df <- get_dataset(dataset_id)
  cols <- find_time_series_cols(df)
  
  if (length(cols$dates) == 0 || length(cols$nums) == 0) {

    stop("no dates")
  }
  
  # Convert and aggregate
  # first date
  d_col <- cols$dates[1]
  
  # Try ymd_hms first
  parsed1 <- ymd_hms(df[[d_col]], quiet = TRUE)
  # If that failed (NA), try mdy
  parsed2 <- mdy(df[[d_col]], quiet = TRUE)
  
  # Combine — use parsed1 where not NA, otherwise parsed2
  df[[d_col]] <- case_when(
    is.na(parsed1) ~ as_date(parsed2), 
    TRUE ~ as_date(parsed1))
  
  df <- df %>% filter(!is.na(df[[d_col]])) 
  
  # 1. Select numeric columns and the date column
  df_sel <- df %>% 
    select(all_of(d_col), cols$nums) %>%
    mutate(date = as_date(.data[[d_col]]))
  
  # 2. Create a complete sequence of dates
  all_dates <- tibble(date = seq(min(df_sel$date), max(df_sel$date), by = "day"))
  
  # 3. Sum numeric columns per date
  df_sum <- df_sel %>%
    group_by(date) %>%
    summarize(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop")
  
  # 4. Count rows per date (if needed)
  df_count <- df_sel %>%
    group_by(date) %>%
    count() %>%
    rename(n_rows = n)
  
  # 5. Fill missing dates with 0 or NA
  df_sum_full <- all_dates %>%
    left_join(df_sum, by = "date") %>%
    replace(is.na(.), 0)   # or leave as NA if preferred
  
  df_count_full <- df_sum_full %>%
    left_join(df_count, by = "date") %>%
    replace(is.na(.), 0)
  
  # Compute seasonality score for each numeric variable
  results <- map_dfr(list(cols$nums, "n_rows"), function(col) {
    ts_data <- df_count_full[[col]]
    score <- seasonality_score(ts_data, frequency = 12)
    tibble(dataset = dataset_id, variable = col, score = score)
  })
  
  return(list(summary_results = results, df_count_full = df_count_full))
}
