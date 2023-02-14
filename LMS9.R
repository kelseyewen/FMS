library(tidyr)
library(tidyverse);library(xlsx)
Pigs <- read.csv("input/Pigs.csv")
Pigs <- Pigs[Pigs$SectorID =="Pigs",]
Pigs[is.na(Pigs)] <- 99999


Pigs_LMS9 <- count(Pigs, LMS9a)

View(Pigs_LMS9)
LMS9 <- count(Dairy,Dairy[,26], PROV)

df.PigsLMS9 <-data.frame(PROV = c("AB","BC","MB","NB","NS","ON","QC"),
LMS9a = rep(0,7),
LMS9b = rep(0,7),
LMS9c = rep(0,7),
LMS9d = rep(0,7),
LMS9e = rep(0,7),
LMS9f = rep(0,7),
LMS9g = rep(0,7))

for(j in 1:7){
  for (i in 26:32){
    df.PigsLMS9[j,i-24] <-sum(Pigs[,i] == 1 & Pigs$PROV == df.PigsLMS9[j,1])
  }
}
Pigs_LMS9 <- df.PigsLMS9

#Save as a data frame
Pigs_LMS9 <- as.data.frame(Pigs_LMS9)

View(Dairy_LMS8)

#Export tables to excel
xlsx.list <- list(Pigs_LMS9=Pigs_LMS9)
for (i in 1:1) {
  write.xlsx(as.data.frame(Pigs_LMS9),
             file = "results/Pigs.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}