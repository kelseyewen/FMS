# Load the required library
library(dplyr)
library(tidyr)

# Read in your data set
data <- read.csv("your_data.csv")

# Count the responses to the multiple choice question
count_responses <- Beef %>%
  group_by(PROV, SMS5) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$PROV, count_responses$SMS5), ]

# Create a pivot table to show the results organized by province and answer chosen
Beef_SMS5 <- count_responses %>%
  spread(SMS5, count)

# View the resulting pivot table
View(Beef_SMS5)