################################################################################
#Load in required packages
################################################################################
require(tidyverse)

################################################################################
#Set working directory and path for output
################################################################################
setwd("C:/Users/ColemanB/OneDrive - AGR-AGR/Documents/12_Manuscripts/Manure Storage FMS 2024/Data/")
output_dir <- "C:/Users/ColemanB/OneDrive - AGR-AGR/Documents/12_Manuscripts/Manure Storage FMS 2024/Output/"


###############################################################################
#CATTLE
###############################################################################

#read in data
cattle_nums <- read.csv(file = '3210013001_databaseLoadingData.csv')

#Subset to reference periods of interest
cattle_nums_fms <- cattle_nums[cattle_nums$REF_DATE %in% c(2017, 2021), ]

cattle_nums_fms$Year <- ifelse(cattle_nums_fms$REF_DATE == '2017', '2017',
                               ifelse(cattle_nums_fms$REF_DATE == '2021', '2021', NA))

#create new vars
cattle_nums_fms$Month <- ifelse(cattle_nums_fms$Survey.date == 'At January 1', 'Jan',
                                   ifelse(cattle_nums_fms$Survey.date == 'At July 1', 'Jul', NA))

cattle_nums_fms$Operation_Type <- ifelse(cattle_nums_fms$Farm.type == 'On dairy operations', 'Dairy',
                                         ifelse(cattle_nums_fms$Farm.type == 'On beef operations', 'Beef', 'All'))

cattle_nums_fms <- cattle_nums_fms %>%
  mutate(Livestock_Class = case_when(
    Livestock == "Total cattle" ~ "Total",
    Livestock == "Bulls, 1 year and over" ~ "Bulls",
    Livestock == "Dairy cows" ~ "Dairy_cows",
    Livestock == "Beef cows" ~ "Beef_cows",
    Livestock == "Total heifers" ~ "Heifers_total",
    Livestock == "Heifers for dairy replacement" ~ "Dairy_heifers",
    Livestock == "Total beef heifers" ~ "Beef_heifers",
    Livestock == "Heifers for beef replacement" ~ "Beef_heifers_replacement",
    Livestock == "Heifers for slaughter" ~ "Beef_heifers_slaughter",
    Livestock == "Steers, 1 year and over" ~ "Steers",
    Livestock == "Calves, under 1 year" ~ "Calves",
    TRUE ~ NA_character_  # This will assign NA to any unmatched categories
  ))

################################################################################
#Cattle counts
################################################################################

#calculate total dairy animals
dairy_totals <- cattle_nums_fms %>%
  filter((Operation_Type == "Dairy" & Livestock_Class %in% c("Bulls", "Dairy_cows", "Dairy_heifers", "Steers", "Calves")) |
           (Operation_Type == "Beef" & Livestock_Class %in% c("Dairy_cows", "Dairy_heifers"))) %>%
  group_by(GEO, Year, Month) %>%
  summarize(Total_Dairy = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
  group_by(GEO, Year) %>%
  summarize(Avg_Total_Dairy = mean(Total_Dairy, na.rm = TRUE), .groups = "drop")

#calculate total beef animals
beef_totals <- cattle_nums_fms %>%
  filter(Operation_Type == "Beef" & 
           Livestock_Class %in% c("Bulls", "Beef_cows", "Beef_heifers", "Steers", "Calves")) %>%
  group_by(GEO, Year, Month) %>%
  summarize(Total_Beef = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
  group_by(GEO, Year) %>%
  summarize(Avg_Total_Beef = mean(Total_Beef, na.rm = TRUE), .groups = "drop")

#combine the results
final_totals <- full_join(dairy_totals, beef_totals, by = c("GEO", "Year"))

#rename provinces and reorder
province_order <- c("BC", "AB", "SK", "MB", "ON", "QC", "CA")
province_names <- c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec", "Canada")

final_totals <- final_totals %>%
  mutate(Province = case_when(
    GEO == "British Columbia" ~ "BC",
    GEO == "Alberta" ~ "AB",
    GEO == "Saskatchewan" ~ "SK",
    GEO == "Manitoba" ~ "MB",
    GEO == "Ontario" ~ "ON",
    GEO == "Quebec" ~ "QC",
    GEO == "Canada" ~ "CA",
    TRUE ~ GEO
  )) %>%
  filter(Province %in% province_order) %>%
  arrange(match(Province, province_order))

#reshape the data to create Year as a subheading
final_table <- final_totals %>%
  pivot_longer(cols = c(Avg_Total_Dairy, Avg_Total_Beef),
               names_to = "Category",
               values_to = "Value") %>%
  pivot_wider(names_from = c(Category, Year),
              values_from = Value) %>%
  select(Province, 
         Dairy_2017 = Avg_Total_Dairy_2017, 
         Dairy_2021 = Avg_Total_Dairy_2021, 
         Beef_2017 = Avg_Total_Beef_2017, 
         Beef_2021 = Avg_Total_Beef_2021)

write_csv(final_table, file.path(output_dir, "cattle_counts.csv"))


###############################################################################
#POULTRY
###############################################################################

#read in data
poultry_2016 <- read.csv(file = '3210042801_databaseLoadingData_plt_2016.csv')
poultry_2021 <- read.csv(file = '3210037401_databaseLoadingData_plt_2021.csv')

#combine the datasets
combined_data <- bind_rows(poultry_2016, poultry_2021)

#function to clean GEO names
clean_geo_names <- function(geo_string) {
  str_replace(geo_string, " \\[.*\\]", "")
}

#apply the cleaning function to the GEO column
combined_data <- combined_data %>%
  mutate(GEO = clean_geo_names(GEO))

#rename year to coincide with periods of interest
combined_data$Year <- ifelse(combined_data$REF_DATE == '2016', '2017',
                             ifelse(combined_data$REF_DATE == '2021', '2021', NA))

################################################################################
#Poultry counts
################################################################################

total_poultry <- combined_data %>%
  filter(Poultry.inventory %in% c('Total hens and chickens', 'Turkeys', 'Ducks', 'Geese', 'Other poultry')) %>%
  group_by(GEO, Year) %>%
  summarize(Total_Poultry = sum(VALUE, na.rm = TRUE)/1000, .groups = 'drop') %>% #divide totals by 1000 for more manageable totals in the final table
  pivot_wider(names_from = Year, values_from = Total_Poultry, names_prefix = "Poultry_")

#define the mapping of province names to acronyms
province_acronyms <- c(
  "British Columbia" = "BC",
  "Alberta" = "AB",
  "Saskatchewan" = "SK",
  "Manitoba" = "MB",
  "Ontario" = "ON",
  "Quebec" = "QC",
  "New Brunswick" = "NB",
  "Nova Scotia" = "NS",
  "Canada" = "CA"
)

#define the desired order of provinces
province_order <- names(province_acronyms)

#rearrange data frame
total_poultry <- total_poultry %>%
  mutate(GEO = factor(GEO, levels = province_order)) %>%
  arrange(GEO) %>%
  filter(GEO %in% province_order) %>%
  mutate(Province = province_acronyms[as.character(GEO)]) %>%
  select(Province, everything(), -GEO)

write_csv(total_poultry, file.path(output_dir, "poultry_counts.csv"))


###############################################################################
#PIGS
###############################################################################

#read in data
pigs <- read.csv(file = '3210016001_databaseLoadingData_pigs.csv')

#Subset to periods of interest and create Year var
pigs_fms <- pigs[pigs$REF_DATE %in% c(2017, 2021), ]

pigs_fms$Year <- ifelse(pigs_fms$REF_DATE == '2017', '2017',
                             ifelse(pigs_fms$REF_DATE == '2021', '2021', NA))

#create new vars
pigs_fms$Month <- ifelse(pigs_fms$Survey.date == 'At January 1', 'Jan',
                                ifelse(pigs_fms$Survey.date == 'At July 1', 'Jul', NA))

################################################################################
#Pig counts
################################################################################

total_pigs <- pigs_fms %>%
  filter(Livestock == 'Hogs, total') %>%
  group_by(GEO, Year, Month) %>%
  summarize(Total_Pigs= sum(VALUE, na.rm = TRUE), .groups = 'drop') %>%
  group_by(GEO, Year) %>%
  summarize(Avg_Total_Pigs = mean(Total_Pigs, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Avg_Total_Pigs, names_prefix = "Swine_")
head(total_pigs)
  
   
#define mapping of province names to acronyms
province_acronyms <- c(
  "British Columbia" = "BC",
  "Alberta" = "AB",
  "Saskatchewan" = "SK",
  "Manitoba" = "MB",
  "Ontario" = "ON",
  "Quebec" = "QC",
  "New Brunswick" = "NB",
  "Nova Scotia" = "NS",
  "Canada" = "CA"
)

#define the desired order of provinces
province_order <- names(province_acronyms)

#rearrange data frame
total_pigs <- total_pigs %>%
  mutate(GEO = factor(GEO, levels = province_order)) %>%
  arrange(GEO) %>%
  filter(GEO %in% province_order) %>%
  mutate(Province = province_acronyms[as.character(GEO)]) %>%
  select(Province, everything(), -GEO)

write_csv(total_pigs, file.path(output_dir, "swine_counts.csv"))


################################################################################
# COMBINE ALL DATA
################################################################################

setwd("C:/Users/ColemanB/OneDrive - AGR-AGR/Documents/12_Manuscripts/Manure Storage FMS 2024/Output")

#read the CSV files
cattle <- read_csv("cattle_counts.csv")
poultry <- read_csv("poultry_counts.csv")
swine <- read_csv("swine_counts.csv")

#combine all dataframes
combined_data <- cattle %>%
  full_join(poultry, by = "Province") %>%
  full_join(swine, by = "Province")

#replace zeros with NA
combined_data[combined_data == 0] <- NA

#reorder columns
combined_data <- combined_data %>%
  select(Province, Beef_2017, Beef_2021, Dairy_2017, Dairy_2021, 
         Poultry_2017, Poultry_2021, Swine_2017, Swine_2021)

#define desired order of provinces
province_order <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "CA")

#reorder rows based on the desired province order
combined_data <- combined_data %>%
  mutate(Province = factor(Province, levels = province_order)) %>%
  arrange(Province)

#save results to a new CSV file
write_csv(combined_data, "livestock_numbers_statcan.csv")

