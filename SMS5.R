library(readxl)
Beef <- subset(LivestockFarms, SectorID=="Beef", select=SectorID:WGTLIVE_AAFC)
View(Beef)
Dairy <- subset(LivestockFarms, SectorID=="Dairy", select=SectorID:WGTLIVE_AAFC)
Pigs <- subset(LivestockFarms, SectorID=="Pigs", select=SectorID:WGTLIVE_AAFC)
Poultry <- subset(LivestockFarms, SectorID=="Poultry", select=SectorID:WGTLIVE_AAFC)
View(Dairy)
View(Pigs)
View(Poultry)
Dairy <- Dairy[!is.na(Dairy$PROV),]


library(dplyr)

# Count the responses to the multiple choice question
count_responses <- Beef %>%
  group_by(PROV, SMS5) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$PROV, count_responses$SMS5), ]

# Print the resulting table
print(count_responses)


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

# Print the resulting pivot table
print(pivot_table)

