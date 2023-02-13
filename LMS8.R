library(dplyr)
library(tidyr)
library(tidyverse);library(xlsx)

# Count the responses to the multiple choice question
count_responses <- Dairy %>%
  group_by(PROV, LMS8) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$PROV, count_responses$LMS8), ]

# Create a pivot table to show the results organized by province and answer chosen
Dairy_LMS8 <- count_responses %>%
  spread(LMS8, count)
print(Dairy_LMS8)

#change column names and remove column that shows the number of blank answers
colnames(Dairy_LMS8) <- c("Province", "No Cover", "Concrete", "Structure with Roof", "Straw", "Floating Cover in Contact with Manure", "Other")
Dairy_LMS8 <- Dairy_LMS8[,-8]

#Save as a data frame
Dairy_LMS8 <- as.data.frame(Dairy_LMS8)

View(Dairy_LMS8)

#Export tables to excel
xls.list <- list(Dairy_LMS8=Dairy_LMS8)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy_LMS8),
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}