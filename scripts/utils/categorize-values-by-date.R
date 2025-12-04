
library(tidyverse)


# Label Value By Distribution --------------------------------------------

categorize_by_distribution <- function(value,
                                       x,
                                       labels = c("very low", "low", "medium", "high", "very high"),
                                       probs = seq(0, 1, length.out = 6),
                                       na.rm = TRUE) {
  if (!is.numeric(value) || !is.numeric(x)) {
    stop("Both 'value' and 'x' must be numeric.")
  }
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) stop("Reference vector 'x' is empty after removing NA.")
  if (length(labels) != (length(probs) - 1)) {
    stop("'labels' length must be one less than length(probs).")
  }

  # compute quantile breaks; fallback to seq if quantiles are non-increasing
  qt <- tryCatch(
    stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7),
    error = function(e) rep(NA_real_, length(probs))
  )
  if (any(is.na(qt)) || any(diff(qt) == 0)) {
    brks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length(labels) + 1)
  } else {
    brks <- qt
  }

  # categorize (vectorized)
  cat_fac <- cut(value, breaks = brks, labels = labels, include.lowest = TRUE, right = TRUE)

  # coerce out-of-range values to extremes if cut returned NA
  out <- as.character(cat_fac)
  out[is.na(out) & value <= min(brks, na.rm = TRUE)] <- labels[1]
  out[is.na(out) & value >= max(brks, na.rm = TRUE)] <- labels[length(labels)]

  factor(out, levels = labels, ordered = TRUE)
}



