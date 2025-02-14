################################################################################
#Load in required packages
################################################################################
require(tidyverse)

################################################################################
#Set working directory
################################################################################
setwd("//skyemci05-eo.efs.agr.gc.ca/projects/farmManagementSurvey/Brent/Livestock_Numbers/")

################################################################################
#Reading in data
################################################################################
statcan_data <- read_csv("C:/Users/ColemanB/OneDrive - AGR-AGR/Documents/12_Manuscripts/Manure Storage FMS 2024/Output/livestock_numbers_statcan.csv")
fms_data <- read_csv("livestock_numbers_fms.csv")
counts_data <- read_csv("livestock_respondents_fms.csv")

#merge the livestock numbers data from fms and statcan counts
merged_data <- left_join(fms_data, statcan_data, by = "Province", suffix = c("_fms", "_statcan"))

#define the columns we want to process
columns <- c("Beef_2017", "Beef_2021", "Dairy_2017", "Dairy_2021", 
             "Poultry_2017", "Poultry_2021", "Swine_2017", "Swine_2021")

################################################################################
#Calculate Percentage of Statcan Livestock Represented by Wtd FMS Livestock
################################################################################

#function to format the data
format_data <- function(fms_value, statcan_value) {
  if (!is.na(fms_value) && !is.na(statcan_value) && statcan_value != 0) {
    percentage <- (fms_value / statcan_value) * 100 
    if (percentage >= 100) {
      return(sprintf("%.0f (~100%%)", fms_value)) #assign value of ~100% if percentage > 100
    } else {
      return(sprintf("%.0f (%.0f%%)", fms_value, percentage))
    }
  } else {
    return(sprintf("%.0f (-)", fms_value)) #assign value of - if provincial data unavailable
  }
}

#apply formatting
for (col in columns) {
  merged_data[[col]] <- mapply(format_data, 
                               merged_data[[paste0(col, "_fms")]], 
                               merged_data[[paste0(col, "_statcan")]])
}

#select needed columns
result <- merged_data %>% select(Province, all_of(columns))

#define order of provinces
province_order <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "CA")

#sort the dataframe based on the custom order
result <- result %>%
  mutate(Province_order = match(Province, province_order)) %>%
  arrange(Province_order) %>%
  select(-Province_order)

#save the result to a new CSV file
write_csv(result, "table2_livestock_counts_only.csv")


################################################################################
# Add Respondent Counts Data to Table 2
################################################################################

#prepare counts_data
counts_data <- counts_data %>%
  mutate(Province_order = match(Province, province_order)) %>%
  arrange(Province_order) %>%
  select(-Province_order)

#convert counts_data to character type for consistency
counts_data <- counts_data %>%
  mutate(across(all_of(columns), as.character))

#define the desired order of provinces
province_order <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "CA")

#create a new dataframe with alternating rows
final_result <- counts_data %>%
  mutate(Province = factor(Province, levels = province_order)) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -c(Province, row_id), names_to = "Variable", values_to = "Value") %>%
  mutate(Data_type = "n") %>%
  bind_rows(
    result %>%
      mutate(Province = factor(Province, levels = province_order)) %>%
      mutate(row_id = row_number()) %>%
      pivot_longer(cols = -c(Province, row_id), names_to = "Variable", values_to = "Value") %>%
      mutate(Data_type = "Wtd Animals")
  ) %>%
  arrange(Province, row_id, desc(Data_type)) %>%
  select(-row_id) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  select(Province, Data_type, all_of(columns))

#print the final result
print(final_result)

#save the result to a new CSV file
write_csv(final_result, "table2_new.csv")
