# Load the required library
library(dplyr)
library(tidyr)

# Count the responses to the multiple choice question
count_responses <- Poultry %>%
  group_by(PROV, SMS5) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$PROV, count_responses$SMS5), ]

# Create a pivot table to show the results organized by province and answer chosen
Poultry_SMS5 <- count_responses %>%
  spread(SMS5, count)

#change column names and remove column that shows the number of blank answers
colnames(Poultry_SMS5) <- c("Province", "<6 months", "6-12 months", "1-2 Years", ">2 Years")
Poultry_SMS5 <- Poultry_SMS5[,-6]

#Save as a data frame
Poultry_SMS5 <- as.data.frame(Poultry_SMS5)

View(Poultry)