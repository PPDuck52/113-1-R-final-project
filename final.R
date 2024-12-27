library(tidyverse)

raw_data <- read.csv("googleplaystore.csv")
#view(raw_data)
#CLEAN DATA
cleaned_data <- raw_data |>
  dplyr::filter(
    !is.na(Rating),
    !is.na(Type),
    !is.na(`Content.Rating`),
    !is.na(`Current.Ver`),
    !is.na(`Android.Ver`)
  )

cleaned_data <- cleaned_data[-c(9118), ]
cleaned_data <- cleaned_data |> 
  select(-Size, -Last.Updated, -Current.Ver, -Android.Ver, -Genres)

#glimpse(cleaned_data)

#mutate variables
library(dplyr)
library(lubridate)
library(stringr)

prepared_data <- cleaned_data |>
  dplyr::mutate(
    # Clean Installs column to numeric
    installs_numeric = as.numeric(stringr::str_remove_all(Installs, "[+,]")),
    
    # Categorize apps as Free or Paid based on Price
    price_category = ifelse(Price == "0", "Free", "Paid"),
    
    # Log-transform Reviews (add 1 to avoid log(0))
    log_reviews = log(as.numeric(Reviews) + 1)
    
  )

view(prepared_data)
glimpse(prepared_data)
