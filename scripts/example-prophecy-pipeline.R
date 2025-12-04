library(purrr)
library(tidyverse)
source(here::here("scripts", "utils", "analyze-and-process-cycles.R"))
source(here::here("scripts", "utils", "categorize-values-by-date.R"))
source(here::here("scripts", "utils", "generate-prophecy.R"))
source(here::here("scripts", "utils", "get-datasets-and-metadata.R"))

dataset_id <- "tg4x-b46p"

example_date <- "2025-06-18"


# Get Metadata -----------------------------------------------------------
meta <- get_dataset_metadata(dataset_id) %>%
  select(dataset = name)

# Run Process and Check Cyclicality --------------------------------------------------

res <- process_cycles(dataset_id)
res$summary_results
df_count_full <- res$df_count_full


# * Plot Time Series -------
df_count_full %>%
  # filter(date > "2021-01-01") %>% 
  # filter(date < "2023-01-01") %>% 
  ggplot(., aes(x = date, y = n_rows)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily Time Series", x = "Date", y = "Value") +
  theme_minimal() + 
  scale_x_date(breaks = "weeks") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Categorize Values For Input Date -----------------------------------------------------------

count_on_date <- df_count_full %>% 
  filter(date == example_date) %>%
  pull(n_rows)

category <- categorize_by_distribution(count_on_date, df_count_full$n_rows)
category


# Generate Prophecy ------------------------------------------------------

# Input data will be replaced with the data generated from the above pipeline, but using 
# example data for now
input_data <- tribble(
  ~dataset,    ~variable,         ~detail,                         ~value,
  "311 Calls", "Number of Calls", "to Environmental Protection",   "High",
  "Weather",   "Temperature",     "in Central Park",               "Low"
)

generate_prophecies(input_data)

# From the heart of the city, a high tide of pleas for nature's shield will surge forth. Though a profound chill settles over Central
# Park, this collective cry hints at a crucial revelation poised to reshape the very air.