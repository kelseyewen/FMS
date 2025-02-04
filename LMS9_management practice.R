#LMS9 is a question regarding 
# Load the required library
library(tidyverse);library(xlsx)

#2017 data, only dairy and pigs have liquid manure
#MTS2:  1 liquid, 2 solid, 3 l and S equal 
Dairy2017 <- read.csv("input/2017/Dairy2017.csv",header=T) %>%
  filter(MTS2 == 1 | MTS2 ==3)
Pigs2017 <- read.csv("input/2017/Pigs2017.csv",header=T) %>%
  filter(MTS2 == 1 | MTS2 ==3)

#2022 data
#MTS2:  1 liquid, 2 solid, 3 l and S equal 
Dairy2022 <- read.csv("input/2022/Dairy2022.csv",header=T) %>%
  filter(MTS02 == 1 | MTS02 ==3)
Pigs2022 <- read.csv("input/2022/Pigs2022.csv",header=T) %>%
  filter(MTS02 == 1 | MTS02 ==3)


#Create a list for the data
LMS9_list <- list(Dairy2017,Dairy2022,Pigs2017,Pigs2022)

#obtain the columne number
coln <- which(colnames(Pigs2022) == "LMS9a"):
  which(colnames(Pigs2022) == "LMS9g") # all livestock are 25-31


prov.name <- c(sort(unique(Dairy2017$prov)),"SK","National")

#A.empty Dairy table
df.LMS9_D <- data.frame(Livestock = rep("Dairy",each = 9),
                      prov = prov.name,
                      Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
                      )
#B.empty Pigs table
df.LMS9_P <- data.frame(Livestock = rep("Pigs",each = 9),
                      prov = prov.name,
                      Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
                      )

#obtain the # farms 
#For Dairy
for (j in 1:2) {
   for (i in 1:8) {
  df.LMS9_D[i,j+2] <- sum(LMS9_list[[j]][,coln] == 1 & 
                       LMS9_list[[j]]$prov == df.LMS9_D$prov[i],
                       na.rm = T)
   }
}
df.LMS9_D[9,3:4] <- colSums(df.LMS9_D[1:8,3:4]) #National number

#For Pigs
for (j in 3:4) {
  for (i in 1:8) {
    df.LMS9_P[i,j] <- sum(LMS9_list[[j]][,coln] == 1 & 
                        LMS9_list[[j]]$prov == df.LMS9_P$prov[i],
                        na.rm = T)
  }
}
df.LMS9_P[9,3:4] <- colSums(df.LMS9_P[1:8,3:4]) #National number


#Calculate the weighted results answer by answer
#there are 8 answers with 2 years so 16 columns
#Dairy
for (j in coln) {
  for (i in 1:2) {
  temp <-
     aggregate(LMS9_list[[i]]$weight~LMS9_list[[i]][,j]+prov,
          data = LMS9_list[[i]], sum)
  df.LMS9_D <- left_join(df.LMS9_D,temp[,2:3],by="prov")
  }
}
df.LMS9_D[is.na(df.LMS9_D)] <- 0
df.LMS9_D[9,5:18] <- colSums(df.LMS9_D[1:8,5:18]) #National number


#Pigs
for (j in coln) {
  for (i in 3:4) {
    temp <-
      aggregate(LMS9_list[[i]]$weight~LMS9_list[[i]][,j]+prov,
                data = LMS9_list[[i]], sum)
    df.LMS9_P <- left_join(df.LMS9_P,temp[,2:3],by="prov")
  }
}
df.LMS9_P[is.na(df.LMS9_P)] <- 0
df.LMS9_P[9,5:18] <- colSums(df.LMS9_P[1:8,5:18]) #National number

#obtain the weight percentage result for weight percentage table
y2017 <- c(5,7,9,11,13,15,17)
y2022 <- c(6,8,10,12,14,16,18)
df.LMS9_D[,y2017] <- round(proportions(as.matrix(df.LMS9_D[,y2017]),1)*100,1)
df.LMS9_D[,y2022] <- round(proportions(as.matrix(df.LMS9_D[,y2022]),1)*100,1)
df.LMS9_P[,y2017] <- round(proportions(as.matrix(df.LMS9_P[,y2017]),1)*100,1)
df.LMS9_P[,y2022] <- round(proportions(as.matrix(df.LMS9_P[,y2022]),1)*100,1)
#replace NaN to 0
df.LMS9_D[is.na(df.LMS9_D)] <- 0
df.LMS9_P[is.na(df.LMS9_P)] <- 0

#combine the two dataframe
df.LMS9 <- rbind(df.LMS9_D,df.LMS9_P)
#add a row for years
df.LMS9 <- rbind(c("","",rep(c("2017","2022"),9)),df.LMS9)

#Change column names
colnames(df.LMS9) <- c("Livestock", "Province",
                       "Farm Response","Farm Response",
                       rep(c("Agitated prior to application",
                       "Aerated to increase oxygen",
                       "Mechanically separated coarse solids",
                       "Mixed with additives",
                       "Anaerobic biodigester or methane capture",
                       "Other", "No practices"),each = 2))


#Export tables to excel
  write.xlsx(df.LMS9,
             file = "results/LMS9_management practice.xlsx",
             sheetName = "LMS9_management practice",
             row.names = F, col.names=TRUE)

