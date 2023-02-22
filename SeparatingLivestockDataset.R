library(tidyr)
library(tidyverse);library(xlsx)
library(dplyr)
#Separate All Livestock Table Into Each Livestock Type
Livestock_2022 <- read.csv("~/FMS/Input/2022livestock.csv")
Beef2022 <- subset(Livestock_2022, FarmType=="Beef", select=QUID:FarmType)
View(Beef2022)
Dairy2022 <- subset(Livestock_2022, FarmType=="Dairy", select=QUID:FarmType)
Pigs2022 <- subset(Livestock_2022, FarmType=="Pigs", select=QUID:FarmType)
Poultry2022 <- subset(Livestock_2022, FarmType=="Poultry", select=QUID:FarmType)
View(Dairy2022)
View(Pigs2022)
View(Poultry2022)

#Find the provinces, StatCan SGC code to Int approved alpha code
prov.code <- data.frame(SGC=c(12,13,24,35,46,47,48,59),
                        alphacode=c("NS","NB","QC","ON","MB","SK","AB","BC"))

for (i in 1:nrow(prov.code)){
  Pigs2022$prov <- replace(Pigs2022$prov,Pigs2022$prov == prov.code[i,1],prov.code[i,2])
}


#Export tables to excel
xls.list <- list(Poultry2022=Poultry2022)
for (i in 1:1) {
  write.xlsx(as.data.frame(Poultry2022),
             file = "input/Poultry2022.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}