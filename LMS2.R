# Load the required library
library(dplyr)
library(tidyr)

# Count the responses to the multiple choice question
count_responses <- Pigs %>%
  group_by(PROV, LMS2) %>%
  summarize(count = n())

# Reorder the columns in the table
count_responses <- count_responses[order(count_responses$PROV, count_responses$LMS2), ]

# Create a pivot table to show the results organized by province and answer chosen
Pigs_LMS2 <- count_responses %>%
  spread(LMS2, count)
print(Pigs_LMS2)

#change column names and remove column that shows the number of blank answers
colnames(Pigs_LMS2) <- c("Province", "Multi-Cell Below Ground Pit", "Earthen Lagoon", "Other Below-Ground Tank", "Above-Ground Tank Outside", "Pit ot Tank Below Floor in Building", "Partial Below Ground Tank Outside", "Other")
Pigs_LMS2 <- Pigs_LMS2[,-9]

#Save as a data frame
Pigs_LMS2 <- as.data.frame(Pigs_LMS2)

View(Pigs_LMS2)

#Export tables to excel
xls.list <- list(Pigs_LMS2=Pigs_LMS2)
for (i in 1:1) {
  write.xlsx(as.data.frame(Pigs_LMS2),
             file = "results/Pigs.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}