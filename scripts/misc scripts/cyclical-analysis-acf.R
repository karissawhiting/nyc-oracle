detect_strongest_labeled_cycle <- function(ts_data, top_n = 5, plot = TRUE) {
  # ts_data: numeric vector of daily values
  # top_n: number of top cycles to report
  # plot: whether to plot ACF and spectrum
  
  # --- 1. ACF plot ---
  if(plot) {
    acf(ts_data, lag.max = 365, main = "ACF: Detect Cycles")
  }
  
  # --- 2. Spectral analysis ---
  spec <- spectrum(ts_data, plot = plot)
  
  # --- 3. Calculate periods and relative power ---
  period <- 1 / spec$freq          # period in days
  power <- spec$spec
  rel_power <- power / sum(power)  # 0-1 relative strength
  
  # --- 4. Identify top N cycles ---
  top_idx <- order(power, decreasing = TRUE)[1:top_n]
  top_cycles <- data.frame(
    period_days = period[top_idx],
    power = power[top_idx],
    relative_strength = rel_power[top_idx]
  )
  
  # --- 5. Label cycles approximately ---
  label_cycle <- function(days) {
    if(abs(days - 7) <= 1) return("weekly")
    if(abs(days - 30) <= 5) return("monthly")
    if(abs(days - 365) <= 10) return("yearly")
    return("other")
  }
  top_cycles$label <- sapply(top_cycles$period_days, label_cycle)
  
  # --- 6. Select the strongest cycle ---
  strongest_cycle <- top_cycles[1, ]
  
  # --- 7. Return results ---
  list(
    top_cycles = top_cycles,           # table of top N cycles
    strongest_cycle = strongest_cycle, # strongest individual cycle
    overall_score = strongest_cycle$relative_strength  # 0-1 strength
  )
}

# --- Example usage ---
ts_data <- df$number_of_persons_killed # your daily series
result <- detect_strongest_labeled_cycle(ts_data, top_n = 5)
result$top_cycles        # table with period, power, relative strength, label
result$strongest_cycle   # strongest individual cycle
result$overall_score     # 0-1 strength score



