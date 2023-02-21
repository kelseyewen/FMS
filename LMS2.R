##Code works for all livestock types, just change the name of the data sheets
# Load the required library
library(dplyr)
library(tidyr)

# Count the responses to the multiple choice question
count_responses <- Dairy2022 %>%
  group_by(prov, LMS02) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$prov, count_responses$LMS02), ]

# Create a pivot table to show the results organized by province and answer chosen
Dairy2022_LMS02 <- count_responses %>%
  spread(LMS02, count)
print(Dairy2022_LMS02)

#change column names and remove column that shows the number of blank answers
colnames(Dairy2022_LMS02) <- c("Province", "Multi-Cell Below Ground Pit", "Earthen Lagoon", "Other Below-Ground Tank", "Above-Ground Tank Outside", "Pit ot Tank Below Floor in Building", "Partial Below Ground Tank Outside", "Other")
Dairy2022_LMS02 <- Dairy2022_LMS02[,-9]

#Save as a data frame
Dairy2022_LMS02 <- as.data.frame(Dairy2022_LMS02)

View(Dairy2022_LMS02)

#Export tables to excel
xls.list <- list(Dairy2022_LMS02=Dairy2022_LMS02)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy2022_LMS02),
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}