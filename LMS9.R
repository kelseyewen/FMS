library(tidyr)
library(tidyverse);library(xlsx)
Dairy2022 <- read.csv("input/Dairy2022.csv")
Dairy2022 <- Dairy2022[Dairy2022$FarmType =="Dairy",]
Pigs2022[is.na(Pigs2022)] <- 99999

#Find the provinces, StatCan SGC code to Int approved alpha code
prov.code <- data.frame(SGC=c(12,13,24,35,46,47,48,59),
                        alphacode=c("NS","NB","QC","ON","MB","SK","AB","BC"))

for (i in 1:nrow(prov.code)){
  Dairy2022$prov <- replace(Dairy2022$prov,Dairy2022$prov == prov.code[i,1],prov.code[i,2])
}


df.Dairy2022LMS9 <-data.frame(prov = c("AB","BC","MB","NB","NS","ON","QC","SK"),
LMS09a = rep(0,8),
LMS09b = rep(0,8),
LMS09c = rep(0,8),
LMS09d = rep(0,8),
LMS09e = rep(0,8),
LMS09f = rep(0,8),
LMS09g = rep(0,8))

for(j in 1:8){
  for (i in 26:32){
    df.Dairy2022LMS9[j,i-24] <-sum(Dairy2022[,i] == 1 & Dairy2022$prov == df.Dairy2022LMS9[j,1])
  }
}
Dairy2022_LMS9 <- df.Dairy2022LMS9

#Change column names
colnames(Dairy2022_LMS9) <- c("Province", "Agitated prior to application", "Aerated to increase oxygen", "Mechanically separated coarse solids", "Mixed with additives", "Anaerobic biodigester or methane capture", "Other", "No practices")

#Save as a data frame
Dairy2022_LMS9 <- as.data.frame(Dairy2022_LMS9)

View(Pigs2022_LMS9)

#Export tables to excel
xlsx.list <- list(Dairy2022_LMS9=Dairy2022_LMS9)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy2022_LMS9),
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}