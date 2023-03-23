# FMS
This is repository for the analysis process and results for Canada farm survey in 2017 and 2022. 

# Data requirement
All data start from original data "2017livestock.csv" and "2022livestock.csv" from Stat Canada. The data is confidential. Anyone who would like to reproduce the results need to send your request to Dr. Andrew VanderZaag (andrew.vanderzaag@AGR.GC.CA). Dr. Andrew VanderZaag is the Research Scientist at Agriculture Agri-Food Canada and responsible for this project. Dr. Chih-Yu Hung (chih-yu.hung@AGR.GC.CA / chih-yu.hung@mail.mcgill.ca) is responsible for the R script, which is based on Ewen Kelsey's analysis in excel sheet. Should you have any issue related to the analysis, please contact Dr. VanderZaag or Dr. Hung.  

# Analysis detail
In this study, we analyzed the management practice, storage time, cover type, and storage time for solid manure (SM) and liquid manure (LM).
The two original data were grouped by Livestock, including Dariy, Beef, Pigs, and Poultry. The grouping process can be found in "SeparatingLivestockDataset.R"

#Soild Manure, SM
For SM, Management practice (SMS4) and Storage time (SMS5) were analyzed. The SMS4 and SMS5 were the code used in the survey. Dairy, Beef, and Poultry data were used here. 
The analysis of SMS4 can be founded in "SMS4_management practice.R"
The analysis of SMS5 can be founded in "SMS5_stroage time.R"

#Liquid Manure, LM
For LM, Storage type (LMS2), Storage time (LMS4), Cover type (LMS8), Management practice (LMS9) were analyzed. The codes were used in the survey. Dairy and Pigs data were used here. 
The analysis of LMS2 can be founded in "LMS2_Storage type.R"
The analysis of LMS4 can be founded in "LMS4_Storage time.R"
The analysis of LMS8 can be founded in "LMS8_cover type.R"
The analysis of LMS9 can be founded in "LMS9_management.R"

