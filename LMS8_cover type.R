#LMS8 is a question regarding cover type
# Load the required library
library(tidyverse);library(xlsx)

#2017 data, only dairy and pigs have liquid manure
Dairy2017 <- read.csv("input/2017/Dairy2017.csv",header=T)
Pigs2017 <- read.csv("input/2017/Pigs2017.csv",header=T)

#2022 data
Dairy2022 <- read.csv("input/2022/Dairy2022.csv",header=T)
Pigs2022 <- read.csv("input/2022/Pigs2022.csv",header=T)

#Create a list for the data
LMS8_list <- list(Dairy2017,Dairy2022,Pigs2017,Pigs2022)
prov.name <- c(sort(unique(Dairy2017$prov)),"SK","National")


#A.empty Dairy table
df.LMS8_D <- data.frame(Livestock = rep("Dairy",each = 9),
                        prov = prov.name,
                        Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
                        )
#B.empty Pigs table
df.LMS8_P <- data.frame(Livestock = rep("Pigs",each = 9),
                        prov = prov.name,
                        Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
                        )

#obtain the # farms 
#For Dairy
for (j in 1:2) {
  for (i in 1:8) {
    df.LMS8_D[i,j+2] <- sum(LMS8_list[[j]]$LMS8 > 0 & 
                              LMS8_list[[j]]$prov == df.LMS8_D$prov[i],
                            na.rm = T)
  }
}
df.LMS8_D[9,3:4] <- colSums(df.LMS8_D[1:8,3:4]) #National number

#For Pigs
for (j in 3:4) {
  for (i in 1:8) {
    df.LMS8_P[i,j] <- sum(LMS8_list[[j]]$LMS8 > 0 & 
                              LMS8_list[[j]]$prov == df.LMS8_P$prov[i],
                            na.rm = T)
  }
}
df.LMS8_P[9,3:4] <- colSums(df.LMS8_P[1:8,3:4]) #National number


#Calculate the weighted results answer by answer
#there are 4 answers with 2 years so 14 columns
#Dairy
for (i in 1:2) {
temp <- aggregate(LMS8_list[[i]]$weight~LMS8+prov,
               data = LMS8_list[[i]], sum)
#merge the weighted value to the table
df.LMS8_D <- left_join(df.LMS8_D,
                       reshape(temp, idvar = "prov",timevar = "LMS8",direction = "wide" ),
                       by="prov")
}

df.LMS8_D[is.na(df.LMS8_D)] <- 0
#round the number
df.LMS8_D[,5:ncol(df.LMS8_D)] <- 
               round(df.LMS8_D[,5:ncol(df.LMS8_D)],1)
#national number
df.LMS8_D[9,5:ncol(df.LMS8_D)] <- 
  colSums(df.LMS8_D[1:8,5:ncol(df.LMS8_D)])

#Pigs
for (i in 3:4) {
  temp <- aggregate(LMS8_list[[i]]$weight~LMS8+prov,
                    data = LMS8_list[[i]], sum)
  #merge the weighted value to the table
  df.LMS8_P <- left_join(df.LMS8_P,
                         reshape(temp, idvar = "prov",timevar = "LMS8",direction = "wide" ),
                         by="prov")
}

df.LMS8_P[is.na(df.LMS8_P)] <- 0
#round the number
df.LMS8_P[,5:ncol(df.LMS8_P)] <- 
  round(df.LMS8_P[,5:ncol(df.LMS8_P)],1)
#national number
df.LMS8_P[9,5:ncol(df.LMS8_P)] <- 
  colSums(df.LMS8_P[1:8,5:ncol(df.LMS8_P)])

#reorder the columns by column names
df.LMS8_Dw <- cbind(df.LMS8_D[,1:4],
                   df.LMS8_D[,sort(colnames(df.LMS8_D)[5:ncol(df.LMS8_D)])])
df.LMS8_Pw <- cbind(df.LMS8_P[,1:4],
                   df.LMS8_P[,sort(colnames(df.LMS8_P)[5:ncol(df.LMS8_P)])])

#add year
year <- c("","",rep(c("2017","2022"),7))
df.LMS8_Dw <- rbind(year,df.LMS8_Dw)
df.LMS8_Pw <- rbind(year,df.LMS8_Pw)

#make a list for weighted results
LMS8weight_list <- list(df.LMS8_Dw,df.LMS8_Pw)

#change column names
for (i in 1:2) {
    colnames(LMS8weight_list[[i]]) <- 
    c("Livestock","Province",
      "Farm response","",
      "No Cover","",
      "Concrete","",
      "Structure with Roof","",
      "Straw","",
      "Floating Cover in Contact with Manure","",
      "Other","")
  }


LMS8weight <- rbind(as.data.frame(LMS8weight_list[[1]]),
                    as.data.frame(LMS8weight_list[[2]]))

#Export tables to excel
write.xlsx(LMS8weight,
             file = "results/LMS8_cover type.xlsx",
             sheetName = "weighted values by province", 
             row.names = F, col.names = T, append = TRUE)


#calculate the weighted % results
df.LMS8_D[,5:10] <- round(proportions(
                           as.matrix(df.LMS8_D[,5:10]),1)*100,1)
df.LMS8_D[,11:15] <- round(proportions(
                          as.matrix(df.LMS8_D[,11:15]),1)*100,1)
df.LMS8_P[,5:10] <- round(proportions(
                          as.matrix(df.LMS8_P[,5:10]),1)*100,1)
df.LMS8_P[,11:15] <- round(proportions(
                          as.matrix(df.LMS8_P[,11:15]),1)*100,1)
#replace NaN to 0
df.LMS8_D[is.na(df.LMS8_D)] <- 0
#replace NaN to 0
df.LMS8_P[is.na(df.LMS8_P)] <- 0

#reorder the columns by column names
df.LMS8_D <- cbind(df.LMS8_D[,1:4],
                   df.LMS8_D[,sort(colnames(df.LMS8_D)[5:ncol(df.LMS8_D)])])
df.LMS8_P <- cbind(df.LMS8_P[,1:4],
                   df.LMS8_P[,sort(colnames(df.LMS8_P)[5:ncol(df.LMS8_P)])])

#add year
df.LMS8_D <- rbind(year,df.LMS8_D)
df.LMS8_P <- rbind(year,df.LMS8_P)

LMS8wp_list <- list(df.LMS8_D,df.LMS8_P)
#change column names
for (i in 1:2) {
  colnames(LMS8wp_list[[i]]) <- 
    c("Livestock","Province",
      "Farm response","",
      "No Cover","",
      "Concrete","",
      "Structure with Roof","",
      "Straw","",
      "Floating Cover in Contact with Manure","",
      "Other","")
}


LMS8wp <- rbind(as.data.frame(LMS8wp_list[[1]]),
                    as.data.frame(LMS8wp_list[[2]]))

#write the result
write.xlsx(LMS8wp,
             file = "results/LMS8_cover type.xlsx",
             sheetName = "wetight percentage by province",
           row.names = F, col.names = T, append = TRUE)
