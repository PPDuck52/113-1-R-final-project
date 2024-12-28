#https://chatgpt.com/share/676ff376-1ef8-8008-a275-f41405b40930
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

raw_data <- read.csv("googleplaystore.csv")
#view(raw_data)
reviews_data <- read.csv("googleplaystore_user_reviews.csv")
#view(reviews_data)

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

#---ADJUSTING---
#step 1. remove the noise information (rows) from reviews_data
subjectivity_value = 0.7    #parameter determined by value of subjectivity
polarity_postive = 0.6
polarity_negative = -0.6
reviews_data <- reviews_data |>
  dplyr::filter(
    !is.na(App),
    !is.na(Translated_Review),
    !is.na(Sentiment),
    !is.na(Sentiment_Polarity),
    !is.na(Sentiment_Subjectivity),
    Sentiment_Subjectivity >= subjectivity_value,
    !(Sentiment_Polarity < polarity_postive &
    Sentiment_Polarity > polarity_negative)
  )

view(reviews_data)

#mutate variables
prepared_data <- cleaned_data |>
  dplyr::mutate(
    # Clean Installs column to numeric
    installs_numeric = as.numeric(stringr::str_remove_all(Installs, "[+,]")),
    
    # Categorize apps as Free or Paid based on Price
    price_category = ifelse(Price == "0", "Free", "Paid"),
    
    # Log-transform Reviews (add 1 to avoid log(0))
    log_reviews = log(as.numeric(Reviews) + 1)
    
  )

#view(prepared_data)
#glimpse(prepared_data)

prepared_data |>
  ggplot(aes(x = fct_infreq(Category))) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Distribution of Apps by Category", x = "Category", y = "Count")


combined_data <- prepared_data |>
  dplyr::left_join(reviews_data, by = "App")

sentiment_summary <- combined_data |>
  dplyr::group_by(App) |>
  dplyr::summarize(
    avg_sentiment_polarity = mean(Sentiment_Polarity, na.rm = TRUE),
    avg_sentiment_subjectivity = mean(Sentiment_Subjectivity, na.rm = TRUE),
    rating = first(Rating),
    .groups = "drop"
  )

#---Sentiment Polarity vs Rating BEFORE ADJUSTING---
# sentiment_summary |>
#   ggplot(aes(x = avg_sentiment_polarity, y = rating)) +
#   geom_point(alpha = 0.5, color = "blue") +
#   geom_smooth(method = "lm", color = "red", se = FALSE) +
#   labs(
#     title = "Sentiment Polarity vs Rating",
#     x = "Average Sentiment Polarity",
#     y = "Rating"
#   ) +
#   theme_minimal()
# 
# correlation <- cor(
#   sentiment_summary$avg_sentiment_polarity,
#   sentiment_summary$rating,
#   use = "complete.obs"
# )
# print(correlation)

#---Sentiment Polarity vs Rating AFTER ADJUSTING---
adjusted_combined_data <- combined_data |>
  dplyr::mutate(
    alpha = 0.5, # Adjust these parameters
    beta = 0.8,  # Adjust these parameters
    weighted_polarity_adjusted = alpha * Sentiment_Polarity * (1 - beta * Sentiment_Subjectivity)
  )

weighted_summary_adjusted <- adjusted_combined_data |>
  dplyr::group_by(App) |>
  dplyr::summarize(
    avg_weighted_polarity_adjusted = mean(weighted_polarity_adjusted, na.rm = TRUE),
    rating = first(Rating),
    .groups = "drop"
  )

weighted_summary_adjusted |>
  ggplot(aes(x = avg_weighted_polarity_adjusted, y = rating)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Sentiment Polarity vs Rating",
    x = "Weighted Average Sentiment Polarity",
    y = "Rating"
  ) +
  theme_minimal()

correlation_adjusted <- cor(
  weighted_summary_adjusted$avg_weighted_polarity_adjusted,
  weighted_summary_adjusted$rating,
  use = "complete.obs"
)

print(correlation_adjusted)
