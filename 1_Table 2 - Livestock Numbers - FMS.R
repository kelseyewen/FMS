################################################################################
#Load in required packages
################################################################################
require(tidyverse)

################################################################################
#Set working directory and path for output for 2021 Data
################################################################################
setwd("//skyemci05-eo.efs.agr.gc.ca/projects/farmManagementSurvey/FMS 2022/FMS 2021 release/Excel files/")
output_dir <- "//skyemci05-eo.efs.agr.gc.ca/projects/farmManagementSurvey/Brent/Livestock_Numbers/"

################################################################################
#Reading in data
################################################################################
#read in all data (except field crop and hort data)
allfarms <- read.csv(file = 'allfarms.csv')
beeffarms <- read.csv(file = 'beeffarms.csv')
dairyfarms <- read.csv(file = 'dairyfarms.csv')
livestockfarms <- read.csv(file = 'livestockfarms.csv')
pigsfarms <- read.csv(file = 'pigsfarms.csv')
poultryfarms <- read.csv(file = 'poultryfarms.csv')

################################################################################
#Data Merging
################################################################################
#function to find common variables between two data frames
common_vars <- function(df1, df2) {
  intersect(names(df1), names(df2))
}

##merging livestock data
#list of all livestock data frames
livestock_farms <- list(beeffarms, dairyfarms, pigsfarms, poultryfarms, livestockfarms)

#use reduce to iteratively merge all data frames on their common variables
livestock_df <- Reduce(function(x, y) {
  common_columns <- common_vars(x, y)
  merge(x, y, by=common_columns, all=TRUE)
}, livestock_farms)

common_lvst_vars <- intersect(names(livestock_df), names(allfarms))
livestock_df <- merge(livestock_df, allfarms, by=common_lvst_vars)

################################################################################
#Calculate Animal Totals and Weighted Animal Totals
################################################################################

livestock_df <- livestock_df %>%
  mutate(
    Tot_Animals_2021 = case_when(
      SECTORID == 1 ~ rowSums(select(., DI01a, DI01b, DI01c, DI01d), na.rm = TRUE),
      SECTORID == 2 ~ rowSums(select(., BI01a, BI01b, BI01c, BI01d, BI01e, BI01f), na.rm = TRUE),
      SECTORID == 3 ~ rowSums(select(., PTI01a_1, PTI01b_1, PTI01c_1, PTI01d_1, PTI01e_1, PTI01f_1, PTI01g_1, PTI01h_1), na.rm = TRUE),
      SECTORID == 4 ~ rowSums(select(., PI01a, PI01b, PI01c, PI01d, PI01e), na.rm = TRUE),
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    Wgt_Tot_Animals_2021 = case_when(
      SECTORID == 1 ~ Tot_Animals_2021 * DWEIGHT_DAIRY_FINAL,
      SECTORID == 2 ~ Tot_Animals_2021 * DWEIGHT_BEEF_FINAL,
      SECTORID == 3 ~ Tot_Animals_2021 * DWEIGHT_POULTRY_FINAL,
      SECTORID == 4 ~ Tot_Animals_2021 * DWEIGHT_PIGS_FINAL,
      TRUE ~ NA_real_
    )
  )

################################################################################
#Create Sector_Name variable
################################################################################

livestock_df <- livestock_df %>%
  mutate(
    Sector_Name = case_when(
      SECTORID == 1 ~ "Dairy",
      SECTORID == 2 ~ "Beef",
      SECTORID == 3 ~ "Poultry",
      SECTORID == 4 ~ "Swine",
      TRUE ~ NA_character_  #assign NA to any other SECTORID values
    ),
    Province = case_when(
      prov == 10 ~ "Newfoundland and Labrador",
      prov == 11 ~ "Prince Edward Island",
      prov == 12 ~ "Nova Scotia",
      prov == 13 ~ "New Brunswick",
      prov == 24 ~ "Quebec",
      prov == 35 ~ "Ontario",
      prov == 46 ~ "Manitoba",
      prov == 47 ~ "Saskatchewan",
      prov == 48 ~ "Alberta",
      prov == 59 ~ "British Columbia",
      TRUE ~ NA_character_  #assign NA to any other prov values
    )
  )


################################################################################
#Create Summary Table of Weighted Livestock Counts
################################################################################

#create summary data frame
summary_df <- livestock_df %>%
  group_by(Province, Sector_Name) %>%
  summarise(
    Total_Animals = sum(Tot_Animals_2021, na.rm = TRUE),
    Total_Wgt_Animals = sum(Wgt_Tot_Animals_2021, na.rm = TRUE),
    Respondents = n(),
    .groups = 'drop'
  )

write.csv(summary_df, file = paste0(output_dir, "livestock_counts_fms_2021.csv"), row.names = FALSE)


################################################################################
#Set working directory and path for output for 2017 data
################################################################################
setwd("//skyemci05-eo.efs.agr.gc.ca/projects/farmManagementSurvey/FMS 2017/repaired microdata FMS 2017/")

################################################################################
#Reading in data
################################################################################
#read in all data (except hort data)
allfarms <- read.csv(file = 'allfarms.csv')
beeffarms <- read.csv(file = 'beefFarms.csv')
dairyfarms <- read.csv(file = 'dairyFarms.csv')
livestockfarms <- read.csv(file = 'LivestockFarms.csv')
pigsfarms <- read.csv(file = 'pigsFarms.csv')
poultryfarms <- read.csv(file = 'poultryFarms.csv')

################################################################################
#Data Merging
################################################################################
#function to find common variables between two data frames
common_vars <- function(df1, df2) {
  intersect(names(df1), names(df2))
}

##merging livestock data
#list of all livestock data frames
livestock_farms <- list(beeffarms, dairyfarms, pigsfarms, poultryfarms, livestockfarms)

#use reduce to iteratively merge all data frames on their common variables
livestock_df <- Reduce(function(x, y) {
  common_columns <- common_vars(x, y)
  merge(x, y, by=common_columns, all=TRUE)
}, livestock_farms)

common_lvst_vars <- intersect(names(livestock_df), names(allfarms))
livestock_df <- merge(livestock_df, allfarms, by=common_lvst_vars)


################################################################################
#Calculate Animal Totals and Weighted Animal Totals
################################################################################

livestock_df <- livestock_df %>%
  mutate(
    Tot_Animals_2017 = case_when(
      SectorID == "Dairy" ~ rowSums(select(., DI01a, DI01b, DI01c, DI01d), na.rm = TRUE),
      SectorID == "Beef" ~ rowSums(select(., BI01a, BI01b, BI01c, BI01d, BI01e, BI01f), na.rm = TRUE),
      SectorID == "Poultry" ~ rowSums(select(., PTI01a_1, PTI01b_1, PTI01c_1, PTI01d_1, PTI01e_1, PTI01f_1, PTI01g_1, PTI01h_1), na.rm = TRUE),
      SectorID == "Pigs" ~ rowSums(select(., PI01a, PI01b, PI01c, PI01d, PI01e), na.rm = TRUE),
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    Wgt_Tot_Animals_2017 = case_when(
      SectorID == "Dairy" ~ Tot_Animals_2017 * WGTdairy_AAFC,
      SectorID == "Beef" ~ Tot_Animals_2017 * WGTBEEF_AAFC,
      SectorID == "Poultry" ~ Tot_Animals_2017 * WGTPOULTRY_AAFC,
      SectorID == "Pigs" ~ Tot_Animals_2017 * WGTPIGS_AAFC,
      TRUE ~ NA_real_
    )
  )


################################################################################
#Create Sector_Name variable
################################################################################

livestock_df <- livestock_df %>%
  mutate(
    Sector_Name = case_when(
      SectorID == "Dairy" ~ "Dairy",
      SectorID == "Beef" ~ "Beef",
      SectorID == "Poultry" ~ "Poultry",
      SectorID == "Pigs" ~ "Swine",
      TRUE ~ NA_character_  
    ),
    Province = case_when(
      PROV == 10 ~ "Newfoundland and Labrador",
      PROV == 11 ~ "Prince Edward Island",
      PROV == 12 ~ "Nova Scotia",
      PROV == 13 ~ "New Brunswick",
      PROV == 24 ~ "Quebec",
      PROV == 35 ~ "Ontario",
      PROV == 46 ~ "Manitoba",
      PROV == 47 ~ "Saskatchewan",
      PROV == 48 ~ "Alberta",
      PROV == 59 ~ "British Columbia",
      TRUE ~ NA_character_
    )
  )


################################################################################
#Create Summary Table of Weighted Livestock Counts
################################################################################

#create summary data frame
summary_df <- livestock_df %>%
  group_by(Province, Sector_Name) %>%
  summarise(
    Total_Animals = sum(Tot_Animals_2017, na.rm = TRUE),
    Total_Wgt_Animals = sum(Wgt_Tot_Animals_2017, na.rm = TRUE),
    Respondents = n(),
    .groups = 'drop'  # This drops the grouping after summarization
  )

write.csv(summary_df, file = paste0(output_dir, "livestock_counts_fms_2017.csv"), row.names = FALSE)

################################################################################
#Set working directory and path for data aggregation - 2017 and 2021
################################################################################

#read the CSV files
df_2017 <- read_csv(file.path(output_dir, "livestock_counts_fms_2017.csv"))
df_2021 <- read_csv(file.path(output_dir, "livestock_counts_fms_2021.csv"))

#add 'Year' column to each dataframe
df_2017$Year <- 2017
df_2021$Year <- 2021

#combine the dataframes
df_combined <- bind_rows(df_2017, df_2021)

#divide Total_Wgt_Animals by 1000 to make the numbers more manageable and comparable with StatCan tables
df_combined <- df_combined %>%
  mutate(Total_Wgt_Animals = Total_Wgt_Animals / 1000)

#create all possible combinations and fill missing values with 0
complete_data <- df_combined %>%
  complete(Province, Sector_Name, Year, fill = list(Total_Wgt_Animals = 0))

#reorder the rows (provinces)
province_order <- c("British Columbia" = "BC", "Alberta" = "AB", "Saskatchewan" = "SK", 
                    "Manitoba" = "MB", "Ontario" = "ON", "Quebec" = "QC", 
                    "New Brunswick" = "NB", "Nova Scotia" = "NS")

################################################################################
#Create Aggregated Summary Table of Weighted Livestock Counts 
################################################################################

#create pivot table
pivot_table <- complete_data %>%
  pivot_wider(
    id_cols = Province,
    names_from = c(Sector_Name, Year),
    values_from = Total_Wgt_Animals,
    names_sep = "_"
  ) %>%
  select(Province, Beef_2017, Beef_2021, Dairy_2017, Dairy_2021, 
         Poultry_2017, Poultry_2021, Swine_2017, Swine_2021) %>%
  filter(Province %in% names(province_order)) %>%
  mutate(Province = factor(Province, levels = names(province_order))) %>%
  arrange(Province) %>%
  mutate(Province = province_order[as.character(Province)])

#calculate the total for each sector and year
totals <- pivot_table %>%
  summarise(across(-Province, sum, na.rm = TRUE))
totals$Province <- "CA"

#combine the pivot table with totals
final_table <- bind_rows(pivot_table, totals)

#format the table (excluding the Province column)
final_table[, -1] <- lapply(final_table[, -1], function(x) format(round(x), big.mark = ","))

print(final_table, n = Inf)

#save the table to a CSV file
write_csv(final_table, file.path(output_dir, "livestock_numbers_fms.csv"))

################################################################################
#Create Summary Table for Number of FMS Respondents
################################################################################

#create a pivot table for respondents
respondents_pivot <- complete_data %>%
  pivot_wider(
    id_cols = Province,
    names_from = c(Sector_Name, Year),
    values_from = Respondents,
    names_sep = "_"
  ) %>%
  select(Province, Beef_2017, Beef_2021, Dairy_2017, Dairy_2021, 
         Poultry_2017, Poultry_2021, Swine_2017, Swine_2021) %>%
  filter(Province %in% names(province_order)) %>%
  mutate(Province = factor(Province, levels = names(province_order))) %>%
  arrange(Province) %>%
  mutate(Province = province_order[as.character(Province)])

#replace NA values with 0 in respondents_pivot
respondents_pivot[is.na(respondents_pivot)] <- 0

#calculate the total respondents for each sector and year
respondents_totals <- respondents_pivot %>%
  summarise(across(-Province, sum, na.rm = TRUE))
respondents_totals$Province <- "CA"

#combine the respondents pivot table with totals
respondents_table <- bind_rows(respondents_pivot, respondents_totals)

#format the respondents table (excluding the Province column)
respondents_table[, -1] <- lapply(respondents_table[, -1], function(x) {
  format(round(x), big.mark = ",")
})

#display the respondents_table
print(respondents_table, n = Inf)

#save the table to a CSV file
write_csv(respondents_table, file.path(output_dir, "livestock_respondents_fms.csv"))
