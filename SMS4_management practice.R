#SMS4 is a question regarding management practice
#We have Dairy, Beef, and Poultry here
library(tidyverse);library(xlsx)

#2017 data

Dairy2017 <- read.csv("input/2017/Dairy2017.csv",header=T)
Beef2017 <- read.csv("input/2017/Beef2017.csv",header=T)
Poultry2017 <- read.csv("input/2017/Poultry2017.csv",header=T)

#2022 data
Dairy2022 <- read.csv("input/2022/Dairy2022.csv",header=T)
Beef2022 <- read.csv("input/2022/Beef2022.csv",header=T)
Poultry2022 <- read.csv("input/2022/Poultry2022.csv",header=T)

SMS4_list <- list(Dairy2017,Dairy2022,
                 Beef2017,Beef2022,
                 Poultry2017,Poultry2022)

#obtain the column number
coln <- which(colnames(Dairy2017) == "SMS4a"):
  which(colnames(Dairy2017) == "SMS4f") # all livestock are 43-48

#A.
#National table, SMS4*.w = weight,  SNS4*.n = # farms
df.SMS4 <- data.frame(Livestock = rep(c("Dairy","Beef","Poultry"),each = 2),
                      Year = rep(c("2017","2022"),3),
                      SMS4a.w = rep(0,6), SMS4a.n = rep(0,6),
                      SMS4b.w = rep(0,6), SMS4b.n = rep(0,6),
                      SMS4c.w = rep(0,6), SMS4c.n = rep(0,6),
                      SMS4d.w = rep(0,6), SMS4d.n = rep(0,6),
                      SMS4e.w = rep(0,6), SMS4e.n = rep(0,6),
                      SMS4f.w = rep(0,6), SMS4f.n = rep(0,6)
                      )

#B.
#National table, SMS4*.wp = weight%,  SNS4*.n = # farms
df.SMS4wp <- data.frame(Livestock = rep(c("Dairy","Beef","Poultry"),each = 2),
                        Year = rep(c("2017","2022"),3),
                        SMS4a.wp = rep(0,6), SMS4a.n = rep(0,6),
                        SMS4b.wp = rep(0,6), SMS4b.n = rep(0,6),
                        SMS4c.wp = rep(0,6), SMS4c.n = rep(0,6),
                        SMS4d.wp = rep(0,6), SMS4d.n = rep(0,6),
                        SMS4e.wp = rep(0,6), SMS4e.n = rep(0,6),
                        SMS4f.wp = rep(0,6), SMS4f.n = rep(0,6)
)

#obtain the # farms for weight table
for(j in 1:6) {
  for (i in coln){
    df.SMS4[j,(i-41)*2] <- sum(SMS4_list[[j]][,i] == 1, na.rm = T)
  }
}

#obtain the # farms for weight percentage table
for(j in 1:6) {
  for (i in coln){
    df.SMS4wp[j,(i-41)*2] <- sum(SMS4_list[[j]][,i] == 1, na.rm = T)
  }
}

#obtain the weight 
#multiply weight to the response. 
for (i in 1:6){
SMS4_list[[i]][,coln] <- SMS4_list[[i]][,coln]*SMS4_list[[i]]$weight
}

for (j in 1:6) {
  for (i in coln) {
    df.SMS4[j,((i-41)*2-1)] <- round(sum(SMS4_list[[j]][,i] , na.rm = T),0)
  }
}

#obtain the weight percent
#we can use proportions to get the percentage,
#but need another kind of table
#calculate based on the weight table
for (j in 1:6) {
  for (i in coln) {
    df.SMS4wp[j,((i-41)*2-1)] <- round(df.SMS4[j,((i-41)*2-1)]/
                                       sum(df.SMS4[j,c(3,5,7,9,11,13)]),3)*100
  }
}

#Insert a new row for responses name
df.SMS4 <- rbind(c("Livestock","Year",rep(c("weight","farm #")
                                            ,6)),df.SMS4)
df.SMS4 <- rbind(c("","",rep(c("Occasionally Turned/Mixed",
                               "Actively Composted",
                               "Mixed with Additives", 
                               "Anaerobic digestion", 
                               "Other",
                               "No practices"),each =2)),df.SMS4)
df.SMS4wp <- rbind(c("Livestock","Year",rep(c("weight %","farm #")
                                          ,6)),df.SMS4wp)
df.SMS4wp <- rbind(c("","",rep(c("Occasionally Turned/Mixed",
                               "Actively Composted",
                               "Mixed with Additives", 
                               "Anaerobic digestion", 
                               "Other",
                               "No practices"),each =2)),df.SMS4wp)

#Export tables to excel
xlsx.list <- list(SMS4.weight = df.SMS4, SMS4.weightpercent = df.SMS4wp)
for (i in 1:2) {
  write.xlsx(xlsx.list[i],
             file = "results/SMS4_management practice.xlsx",
             sheetName = names(xlsx.list)[i],
             row.names = F, col.names= F, append = TRUE)}
