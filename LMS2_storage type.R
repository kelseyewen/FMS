##Code works for all livestock types, just change the name of the data sheets
# Load the required library
library(tidyverse)

#2017 data, only dairy and pigs have liquid manure
Dairy2017 <- read.csv("input/2017/Dairy2017.csv",header=T)
Pigs2017 <- read.csv("input/2017/Pigs2017.csv",header=T)

#2022 data
Dairy2022 <- read.csv("input/2022/Dairy2022.csv",header=T)
Pigs2022 <- read.csv("input/2022/Pigs2022.csv",header=T)

#Create a list for the data
LMS2.list <- list(Dairy2017,Dairy2022,Pigs2017,Pigs2022)

#a function to count the responses
multiple_LM2 <- function(x){
  
  
  # Count the responses to the multiple choice question
    count_responses <- x %>%
     group_by(prov, LMS2) %>%
     summarize(count = n())
  # Reorder the columns in the table
    count_responses <- count_responses[order(count_responses$prov, 
                                             count_responses$LMS2), ]
  # Create a pivot table to show the results 
  # organized by province and answer chosen
    LMS2_table <- count_responses %>%
      spread(LMS2, count)
  
  #change column names and remove column that shows the number of blank answers
    colnames(LMS2_table) <- c("Province",
                               "Multi-Cell Below Ground Pit",
                               "Earthen Lagoon",
                               "Other Below-Ground Tank",
                               "Above-Ground Tank Outside",
                               "Pit ot Tank Below Floor in Building",
                               "Partial Below Ground Tank Outside",
                               "Other")
    #Save as a data frame and remove useless column
    LMS2_table <- as.data.frame(LMS2_table[,1:8])

    
  }

#store the results to temp_LMS2
temp_LMS2 <- lapply(LMS2.list,multiple_LM2)



#Export tables to excel
xls.list <- list(Dairy2022_LMS02=Dairy2022_LMS02)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy2022_LMS02),
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}