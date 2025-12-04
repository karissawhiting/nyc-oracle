library(ellmer)
library(glue)
library(tidyverse)

# ------------------------------------------------------------
# Generate Prophecies Using Gemini (for now)
# ------------------------------------------------------------
generate_prophecies <- function(input_data, model = "gemini") {
  
  # Safety check
  if (ncol(input_data) < 1) {
    stop("Input data must have at least one column.")
  }
  
  # Combine all rows into descriptive text
  all_data_text <- apply(input_data, 1, function(row) {
    paste(names(input_data), ": ", row, collapse = "; ")
  }) %>% 
    paste(collapse = "\n")
  
  
  # Construct the prompt
  prompt <- glue("
    Generate a prophecy-like prediction for a given day based on the following data:
    {all_data_text}
  ")

  # Configure chat model
  chat <- chat_google_gemini(
    system_prompt = "
      You are a hybrid oracle and predictive model.
      Using the input data for a given day, craft a prophecy-like prediction.
      Your output should be poetic, mysterious, and evocative, like a fortune teller's words.
      Keep it very concise: maximum two sentences.
      Use details from all the input data, but maintain an air of enigma and poetic flair.
    "
  )
  
  # Generate prophecy per row
   prophecy <- chat$chat(prompt)
   #return(prophecy)
}

# Example usage:
# input_data <- tribble(
#   ~dataset,    ~variable,         ~detail,                         ~value,
#   "311 Calls", "Number of Calls", "to Environmental Protection",   "High",
#   "Weather",   "Temperature",     "in Central Park",               "Low"
# )

# generate_prophecies(input_data)

# From the heart of the city, a high tide of pleas for nature's shield will surge forth. Though a profound chill settles over Central
# Park, this collective cry hints at a crucial revelation poised to reshape the very air.