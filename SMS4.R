library(tidyr)
library(tidyverse);library(xlsx)
Beef <- read.csv("input/Beef.csv")
Beef <- Beef[Beef$SectorID =="Beef",]
Beef[is.na(Beef)] <- 99999

df.BeefSMS4 <-data.frame(PROV = c("AB","BC","MB","NB","NS","ON","QC","SK"),
                         SMS4a = rep(0,8),
                         SMS4b = rep(0,8),
                         SMS4c = rep(0,8),
                         SMS4d = rep(0,8),
                         SMS4e = rep(0,8),
                         SMS4f = rep(0,8))

for(j in 1:8){
  for (i in 44:49){
    df.BeefSMS4[j,i-42] <-sum(Beef[,i] == 1 & Beef$PROV == df.BeefSMS4[j,1])
  }
}
Beef_SMS4 <- df.BeefSMS4

#Save as a data frame
Beef_SMS4 <- as.data.frame(Beef_SMS4)

View(Beef_SMS4)

#Export tables to excel
xlsx.list <- list(Beef_SMS4=Beef_SMS4)
for (i in 1:1) {
  write.xlsx(as.data.frame(Beef_SMS4),
             file = "results/BeefSMS4.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}