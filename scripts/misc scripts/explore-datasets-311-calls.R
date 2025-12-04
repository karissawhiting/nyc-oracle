library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

source(here::here("scripts", "utils.R"))

# NYC Open Data URL
base_url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.json"

# Years you want to sample
years <- 2020:2024


# Pull and Combine Data --------------------------------------------------

# Pull and combine all years
samples <- lapply(years, get_random_sample_for_year)
combined_data <- bind_rows(samples)

# Parse dates cleanly
combined_data <- combined_data %>%
  mutate(
    across(c("created_date", "closed_date"),
           ~ymd_hms(gsub("T", " ", as.character(.x)), quiet = TRUE))
  )

glimpse(combined_data)


# Check result
glimpse(combined_data)
df <- combined_data

df <- df %>%
  mutate(
    across(c("created_date", "closed_date"), 
           ~ymd_hms(.x, quiet = TRUE)))

df$created_date


cts <- df %>% 
  group_by(agency_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 300) %>%
  pull(agency_name)

df_sel <- df %>%
  filter(agency_name %in% cts)



# Visualize --------------------------------------------------------------



# --- Prepare data for line plot ---
# Count how many datasets were created each month
creation_trend <- df_sel %>%
  mutate(month = floor_date(created_date, "month")) %>%
  group_by(month, agency_name) %>%
  summarise(datasets_created = n()) %>%
  arrange(month) 


ggplot(creation_trend, 
       aes(x = month, y = datasets_created, color = agency_name)) +
  geom_line(aes(color = agency_name), size = 1) +
  geom_point(aes(color = agency_name), size = 2) +
  labs(
    title = "NYC Open Data: 311 Calls Over Time",
    x = "Month",
    y = "Number of Calls"
  ) +
  theme_minimal(base_size = 9) + 
  facet_wrap(~agency_name, ncol = 1, scales = "free")+ 
  scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 month")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "bottom")




# * All- Line Plot -----------

# --- Prepare data for line plot ---
# Count how many datasets were created each month
creation_trend <- df %>%
  mutate(month = floor_date(created_date, "month")) %>%
  group_by(month) %>%
  summarise(datasets_created = n()) %>%
  arrange(month) 

# --- Plot line chart ---
ggplot(creation_trend, 
       aes(x = month, y = datasets_created)) +
  geom_line( size = 1) +
  geom_point( size = 2) +
  labs(
    title = "NYC Open Data (Environment): Dataset Creation Over Time",
    x = "Month",
    y = "Number of Datasets Created"
  ) +
  theme_minimal(base_size = 14)  + 
  scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 month")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Seasonality Score -------------------------------------------------------


table(creation_trend$agency_name)
c <- creation_trend %>% 
  filter(agency_name == "Department of Housing Preservation and Development")

c <- creation_trend %>% 
  group_by(agency_name) %>% 
  nest()

results <- map(c$data, function(df) {
  ts_data <- df$datasets_created
  score <- seasonality_score(ts_data, frequency = 12)
  return(score)
 # tibble(dataset = dataset_id, variable = col, score = score)
})
  
c$data[[1]]$datasets_created
# Compute seasonality score for each numeric variable
results <- map_dfr(cols$nums, function(col) {
  ts_data <- df[[col]]
  score <- seasonality_score(ts_data, frequency = 12)
  tibble(dataset = dataset_id, variable = col, score = score)
})
c$datasets_created
ts_data <- df$
score <- seasonality_score(ts_data, frequency = 12)
tibble(dataset = dataset_id, variable = col, score = score)
