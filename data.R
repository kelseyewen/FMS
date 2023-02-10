#This's the script to obtain the results from Canada farm survey
library(tidyverse);library(xlsx)
Dairy <- read.csv("input/dairyFarms.csv",header=T)

#Find the provinces, StatCan SGC code to Int approved alpha code
prov.code <- data.frame(SGC=c(12,13,24,35,46,47,48,59),
                        alphacode=c("NS","NB","QC","ON","MB","SK","AB","BC"))

for (i in 1:nrow(prov.code)){
  Dairy$PROV <- replace(Dairy$PROV,Dairy$PROV == prov.code[i,1],prov.code[i,2])
}

#There are two prov is NA in Dairy. Exclude the two data
Dairy <- Dairy[!is.na(Dairy$PROV),]

#Amount question, Dairy cattle inventory DI01 for the province
Dairy.DI01 <- aggregate(cbind(DI01a,DI01b,DI01c,DI01d)~PROV,data = Dairy, sum)
colnames(Dairy.DI01) <- c("PROV","milkcows","drycows","heifers","calves")

#Yes/No question, e.g. ventilation, DBHM3a and DBHM3b
DBHM3a <- count(Dairy,DBHM3a,PROV)
DBHM3a <- cbind(DBHM3a,DBHM3a[is.na(DBHM3a$DBHM3a),3])[1:length(unique(DBHM3a$PROV)),]
colnames(DBHM3a) <- c("ventilation","PROV","YES","NO/NA")
DBHM3a$percentage <- round(DBHM3a$YES/(DBHM3a$YES+DBHM3a$`NO/NA`),2) 

#multiple choice question
# Count the number of rows where total is equal to 1 for each name
counts <- sapply(c("BC", "AB", "SK"), function(x) {
  sum(df$col2[df$name == x] == 1, na.rm = TRUE)
})
result_df <- data.frame(name = c("BC", "AB", "SK"), count = counts)


#Export tables to excel
xls.list <- list(Dairy.DI01 = Dairy.DI01,
                 DDBHM3a = DBHM3a)
for (i in 1: length(xls.list)) {
  write.xlsx(xls.list[[i]],
             file = "results/Dairy.xlsx",
             sheetName = names(xls.list)[i], row.names = F, append = TRUE)
}