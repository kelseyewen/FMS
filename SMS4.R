library(tidyr)
Dairy <- read.csv("input/Dairy.csv")
Dairy <- Dairy[Dairy$SectorID =="Dairy",]
Beef[is.na(Beef)] <- 99999

df.DairySMS4 <-data.frame(PROV = c("AB","BC","MB","NB","NS","ON","QC","SK"),
                         SMS4a = rep(0,8),
                         SMS4b = rep(0,8),
                         SMS4c = rep(0,8),
                         SMS4d = rep(0,8),
                         SMS4e = rep(0,8),
                         SMS4f = rep(0,8))

for(j in 1:8){
  for (i in 44:49){
    df.DairySMS4[j,i-42] <-sum(Dairy[,i] == 1 & Dairy$PROV == df.DairySMS4[j,1])
  }
}
Dairy_SMS4 <- df.DairySMS4

#Change column names
colnames(Dairy_SMS4) <- c("Province", "Occasionally Turned/Mixed", "Actively Composted", "Mixed with Additives", "Anaerobic digestion", "Other", "No practices")

#Save as a data frame
Dairy_SMS4 <- as.data.frame(Dairy_SMS4)

View(Dairy_SMS4)

#Export tables to excel
xlsx.list <- list(Dairy_SMS4=Dairy_SMS4)
for (i in 1:1) {
  write.xlsx(as.data.frame(Dairy_SMS4),
             file = "results/Dairy_SMS4.xlsx",
             sheetName = names(xls.list)[1], row.names = F, col.names=TRUE, append = TRUE)}