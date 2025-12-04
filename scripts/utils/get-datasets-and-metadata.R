library(httr)
library(jsonlite)
library(purrr)
library(RSocrata)
library(dplyr)
library(lubridate)
library(forecast)
library(tibbletime)
library(readr)

# ================================
#  Load Dataset (CSV)
# ================================
get_dataset <- function(dataset_id) {
  url <- paste0("https://data.cityofnewyork.us/resource/", dataset_id, ".csv")
  read_csv(url, show_col_types = FALSE)
}

# example
# dataset_id <- "nc67-uf89"
# get_dataset(dataset_id)


# ================================
#  Get Metadata By Category
# ================================
get_metadata_by_category <- function(category = NULL) {
  base_url <- "https://data.cityofnewyork.us/api/views.json"
  
  query_list <- list(
    "$select" = "id,name,category,assetType"
  )
  
  if (!is.null(category)) {
    query_list[["$where"]] <- paste0("category = '", category, "'")
  }
  
  response <- GET(base_url, query = query_list)
  stop_for_status(response)
  
  metadata_raw <- content(response, as = "parsed", simplifyVector = TRUE)
  
  tibble(metadata_raw) %>%
    filter(assetType == "dataset") %>%
    select(id, name, category)
}

# Example Usage:
# get_metadata_by_category(category = "Environment")

# ================================
#  Get Metadata for Single Dataset
# ================================

get_dataset_metadata <- function(dataset_id) {
  url <- paste0("https://data.cityofnewyork.us/api/views/", dataset_id, ".json")
  
  response <- GET(url)
  stop_for_status(response)
  
  m <- content(response, as = "parsed", simplifyVector = TRUE)
  
  tibble(
    id          = m$id,
    name        = m$name,
    category    = m$category,
    description = m$description,
    created_at  = as_datetime(m$createdAt / 1000),
    updated_at  = as_datetime(m$rowsUpdatedAt / 1000)
  )
}

# Example Usage:
# get_dataset_metadata("nc67-uf89")

# =====================================
#  Other Useful Data Retrieval Functions
# ======================================


# Function: get records for a given month (partial)
get_data_for_month <- function(year, month, limit = 1000) {
  start_date <- sprintf("%04d-%02d-01T00:00:00", year, month)
  end_date <- sprintf("%04d-%02d-%02dT23:59:59", year, month, days_in_month(month))
  
  res <- GET(base_url, query = list(
    "$where" = paste0("created_date between '", start_date, "' and '", end_date, "'"),
    "$limit" = limit
  ))
  
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  if (length(data) == 0) return(NULL)
  
  data$year <- year
  data$month <- month
  return(data)
}

# Function: get random sample for a whole year
get_random_sample_for_year <- function(year, per_month = 500, final_sample = 2000) {
  message("Pulling data for ", year, "...")
  
  # Pull across months
  monthly <- lapply(1:12, get_data_for_month, year = year, limit = per_month)
  df <- bind_rows(monthly)
  
  if (nrow(df) == 0) return(NULL)
  
  # Random sample across months
  df <- df %>% sample_n(min(nrow(df), final_sample))
  df$year <- year
  return(df)
}