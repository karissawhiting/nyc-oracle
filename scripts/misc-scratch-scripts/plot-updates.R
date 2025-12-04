library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)

# --- Step 1: Fetch all metadata (paged) ---
base_url <- "https://data.cityofnewyork.us/api/views.json"


# Query only datasets (assetType == "dataset")
response <- GET(base_url,
                query = list(
                  "$select" = "id,name,category,assetType,createdAt,rowsUpdatedAt",
                  "$where"  = "assetType = 'dataset'"
                ))
stop_for_status(response)
metadata_raw <- content(response, as = "parsed", simplifyVector = TRUE)


m <- metadata_raw$metadata
agency <- m$custom_fields$`Dataset Information`$Agency

# Convert to data frame and filter datasets
metadata_raw <- metadata_raw %>%
  as_tibble() %>% bind_cols(.,"agency" = agency)

metadata_raw <- metadata_raw %>%
  filter(assetType == "dataset")

metadata <- metadata_raw  %>%
  mutate(
    createdAt = as_datetime(createdAt),
    rowsUpdatedAt = as_datetime(rowsUpdatedAt)
  ) %>%
  select(id, name, category, createdAt, rowsUpdatedAt, agency)

library(tidyr)


# --- Step 3: Plot histogram ---
ggplot(metadata, aes(x = createdAt)) +
  geom_histogram(binwidth = 90 * 24 * 60 * 60, fill = "steelblue",
                 color = "white") +  # bin = 90 days
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "NYC Open Data: Dataset Creation Over Time",
    x = "Dataset Creation Date",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14)

ggplot(metadata, aes(x = createdAt)) +
  geom_histogram(binwidth = 90 * 24 * 60 * 60, fill = "steelblue",
                 color = "white") +  # bin = 90 days
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "NYC Open Data: Dataset Creation Over Time",
    x = "Dataset Creation Date",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14)




# Line Plot -----------------------------

# --- Prepare data for line plot ---
# Count how many datasets were created each month
creation_trend <- metadata %>%
  mutate(month = floor_date(createdAt, "month")) %>%
  group_by(month, category) %>%
  summarise(datasets_created = n()) %>%
  arrange(month) %>%
  filter(!is.na(category))%>%
  filter(category != "NYC BigApps")

# --- Plot line chart ---
ggplot(creation_trend, 
       aes(x = month, y = datasets_created, color = category)) +
  geom_line(aes(color = category), size = 1) +
  geom_point(aes(color = category), size = 2) +
  labs(
    title = "NYC Open Data (Environment): Dataset Creation Over Time",
    x = "Month",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14) + 
  facet_wrap(~category, ncol = 2)


# Updates -----------------------------------------------------------------


# Line Plot -----------------------------

# --- Prepare data for line plot ---
# Count how many datasets were created each month
creation_trend <- metadata %>%
  mutate(month = floor_date(rowsUpdatedAt, "month")) %>%
  group_by(month, category) %>%
  summarise(datasets_updated = n()) %>%
  arrange(month) %>%
  filter(!is.na(category))%>%
  filter(category != "NYC BigApps")

# --- Plot line chart ---
ggplot(creation_trend, 
       aes(x = month, y = datasets_updated, color = category)) +
  geom_line(aes(color = category), size = 1) +
  geom_point(aes(color = category), size = 2) +
  labs(
    title = "NYC Open Data: Dataset Udpdated Over Time",
    x = "Month",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14) + 
  facet_wrap(~category, ncol = 2)



# By Agency -----------------------------------------------------------------


# Line Plot -----------------------------

# --- Prepare data for line plot ---
# Count how many datasets were created each month
creation_trend <- metadata %>%
  mutate(month = floor_date(rowsUpdatedAt, "month")) %>%
  filter(category != "NYC BigApps") %>%
  filter(!is.na(category))%>%
  filter(!is.na(agency)) %>%
  group_by(agency) %>%
  mutate(n_agency = n()) %>%
  group_by(month, agency) %>%
  summarise(datasets_updated = n(), 
            n_agency = n_agency[[1]]) %>%
  arrange(month) %>%
  filter(n_agency > 50) %>%
  mutate(year = year(month))

# --- Plot line chart ---
ggplot(filter(creation_trend, year > 2018), 
       aes(x = month, y = datasets_updated, color = agency)) +
  geom_col(aes(color = agency, stat = "count"), size = 1) +
#  geom_point(aes(color = agency), size = 2) +
  labs(
    title = "NYC Open Data: Dataset Udpdated Over Time",
    x = "Month",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "none") +
  facet_wrap(~agency, ncol = 2) 



# By Agency -----------------------------------------------------------------


# Line Plot -----------------------------

# --- Prepare data for line plot ---
# Count how many datasets were created each month
creation_trend <- metadata %>%
  mutate(month = floor_date(createdAt, "month")) %>%
  filter(category != "NYC BigApps") %>%
  filter(!is.na(category))%>%
  filter(!is.na(agency)) %>%
  group_by(agency) %>%
  mutate(n_agency = n()) %>%
  group_by(month, agency) %>%
  summarise(datasets_updated = n(), 
            n_agency = n_agency[[1]]) %>%
  arrange(month) %>%
  filter(n_agency > 50)

# --- Plot line chart ---
ggplot(creation_trend, 
       aes(x = month, y = datasets_updated, color = agency)) +
  geom_col(aes(color = agency, stat = "count"), size = 1) +
  #  geom_point(aes(color = agency), size = 2) +
  labs(
    title = "NYC Open Data: Dataset Udpdated Over Time",
    x = "Month",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "none") +
  facet_wrap(~agency, ncol = 2)


