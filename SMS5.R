# Load the required library
library(dplyr)
library(tidyr)

# Count the responses to the multiple choice question
count_responses <- Dairy %>%
  group_by(PROV, SMS5) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$PROV, count_responses$SMS5), ]

# Create a pivot table to show the results organized by province and answer chosen
Dairy_SMS5 <- count_responses %>%
  spread(SMS5, count)
print(Dairy_SMS5)

#change column names and remove column that shows the number of blank answers
colnames(Dairy_SMS5) <- c("Province", "<6 months", "6-12 months", "1-2 Years")
Dairy_SMS5 <- Dairy_SMS5[,-5]

#Save as a data frame
Dairy_SMS5 <- as.data.frame(Dairy_SMS5)

View(Dairy_SMS5)

#Export tables to excel
xls.list <- list(Dairy_SMS5=Dairy_SMS5)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy_SMS5),
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}