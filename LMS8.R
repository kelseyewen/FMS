library(dplyr)
library(tidyr)
library(tidyverse);library(xlsx)
Dairy2022 <- read.csv("input/Dairy2022.csv")

# Count the responses to the multiple choice question
count_responses <- Dairy2022 %>%
  group_by(prov, LMS08) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$prov, count_responses$LMS08), ]

# Create a pivot table to show the results organized by province and answer chosen
Dairy2022_LMS8 <- count_responses %>%
  spread(LMS08, count)
print(Dairy2022_LMS8)

#change column names and remove column that shows the number of blank answers
Dairy2022_LMS8 <- Dairy2022_LMS8[,-2]
colnames(Dairy2022_LMS8) <- c("Province", "No Cover", "Concrete", "Structure with Roof", "Straw", "Floating Cover in Contact with Manure", "Other")

#Save as a data frame
Dairy2022_LMS8 <- as.data.frame(Dairy2022_LMS8)

View(Pigs2022_LMS8)

#Export tables to excel
xls.list <- list(Dairy2022_LMS8=Dairy2022_LMS8)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy2022_LMS8),
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}