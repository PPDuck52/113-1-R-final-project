library(tidyverse)

# Import the CSV file
googleplaystore <- read.csv("googleplaystore.csv")

# Display the dataset
glimpse(googleplaystore)

# Count the occurrences of each category and sort them in descending order
category_counts <- googleplaystore %>%
  count(Category, sort = TRUE)

# Display the sorted categories
#view(category_counts)
