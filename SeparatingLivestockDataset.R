library(tidyverse);library(xlsx);library(dplyr)
#Separate All Livestock Table Into Each Livestock Type
Livestock_2017 <- read.csv("input/2017/2017livestock.csv",header = T)
Livestock_2022 <- read.csv("input/2022/2022livestock.csv",header = T)

#Find the provinces, StatCan SGC code to Int approved alpha code
#Only 2022 since 2017 was replaced in Excel
prov.code <- data.frame(SGC=c(12,13,24,35,46,47,48,59),
                        alphacode=c("NS","NB","QC","ON","MB","SK","AB","BC"))
for (i in 1:nrow(prov.code)){
  Livestock_2022$prov <- replace(Livestock_2022$prov,
                                 Livestock_2022$prov == prov.code[i,1],
                                 prov.code[i,2])
}

#2017, we don't need the farm IDs
Beef2017 <- subset(Livestock_2017, SectorID=="Beef", 
                   select=-QuID)
Dairy2017 <- subset(Livestock_2017, SectorID=="Dairy", 
                   select=-QuID)
Pigs2017 <- subset(Livestock_2017, SectorID=="Pigs", 
                   select=-QuID)
Poultry2017 <- subset(Livestock_2017, SectorID=="Poultry",
                   select=-QuID)

#2022, we don't need to farm IDs
Beef2022 <- subset(Livestock_2022, FarmType=="Beef",
                   select=-QUID)
Dairy2022 <- subset(Livestock_2022, FarmType=="Dairy",
                   select=-QUID)
Pigs2022 <- subset(Livestock_2022, FarmType=="Pigs",
                   select=-QUID)
Poultry2022 <- subset(Livestock_2022, FarmType=="Poultry",
                   select=-QUID)

#Change the questions name, remove 0 after SMS* or LMS*
Dairy2017 <- rename(Dairy2017, prov = PROV,
                    weight = WGTLIVE_AAFC)
Beef2017 <- rename(Beef2017, prov = PROV,
                   weight = WGTLIVE_AAFC)
Poultry2017 <- rename(Poultry2017, prov = PROV,
                      weight = WGTLIVE_AAFC)
Pigs2017 <- rename(Pigs2017, prov = PROV,
                   weight = WGTLIVE_AAFC)

Dairy2022 <- rename(Dairy2022, weight = DWEIGHT_LIVE_FINAL,
                   LMS2 = LMS02, LMS4 = LMS04,
                   LMS9a = LMS09a, LMS9b = LMS09b,
                   LMS9c = LMS09c, LMS9d = LMS09d,
                   LMS9e = LMS09e, LMS9f = LMS09f,
                   LMS9g = LMS09g,
                   SMS4a = SMS04a, SMS4b = SMS04b,
                   SMS4c = SMS04c, SMS4d = SMS04d,
                   SMS4e = SMS04e, SMS4f = SMS04f,
                   SMS5 = SMS05)
Beef2022 <- rename(Beef2022, weight = DWEIGHT_LIVE_FINAL,   
                   LMS2 = LMS02, LMS4 = LMS04,
                   LMS9a = LMS09a, LMS9b = LMS09b,
                   LMS9c = LMS09c, LMS9d = LMS09d,
                   LMS9e = LMS09e, LMS9f = LMS09f,
                   LMS9g = LMS09g,
                   SMS4a = SMS04a, SMS4b = SMS04b,
                   SMS4c = SMS04c, SMS4d = SMS04d,
                   SMS4e = SMS04e, SMS4f = SMS04f,
                   SMS5 = SMS05)
Poultry2022 <- rename(Poultry2022, weight = DWEIGHT_LIVE_FINAL, 
                      LMS2 = LMS02, LMS4 = LMS04,
                      LMS9a = LMS09a, LMS9b = LMS09b,
                      LMS9c = LMS09c, LMS9d = LMS09d,
                      LMS9e = LMS09e, LMS9f = LMS09f,
                      LMS9g = LMS09g,
                      SMS4a = SMS04a, SMS4b = SMS04b,
                      SMS4c = SMS04c, SMS4d = SMS04d,
                      SMS4e = SMS04e, SMS4f = SMS04f,
                      SMS5 = SMS05)
Pigs2022 <- rename(Pigs2022, weight = DWEIGHT_LIVE_FINAL, 
                      LMS2 = LMS02, LMS4 = LMS04,
                      LMS9a = LMS09a, LMS9b = LMS09b,
                      LMS9c = LMS09c, LMS9d = LMS09d,
                      LMS9e = LMS09e, LMS9f = LMS09f,
                      LMS9g = LMS09g,
                      SMS4a = SMS04a, SMS4b = SMS04b,
                      SMS4c = SMS04c, SMS4d = SMS04d,
                      SMS4e = SMS04e, SMS4f = SMS04f,
                      SMS5 = SMS05)




#Export tables to excel
#2017 data
list_2017 <- list(Beef2017=Beef2017,
                  Dairy2017=Dairy2017,
                  Pigs2017=Pigs2017,
                  Poultry2017=Poultry2017)
for (i in 1:length(list_2017)) {
  write.csv(as.data.frame(list_2017[[i]]),
            file = paste0("input/2017/",names(list_2017)[[i]],".csv"),
            row.names = F)
}

#2022 data
list_2022 <- list(Beef2022=Beef2022,
                  Dairy2022=Dairy2022,
                  Pigs2022=Pigs2022,
                  Poultry2022=Poultry2022)
for (i in 1:length(list_2022)) {
  write.csv(as.data.frame(list_2022[[i]]),
             file = paste0("input/2022/",names(list_2022)[[i]],".csv"),
             row.names = F)
  }


