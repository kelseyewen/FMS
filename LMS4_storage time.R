#LMS4 is a question regarding storage time
# Load the required library
library(tidyverse);library(xlsx)

#2017 data, only dairy and pigs have liquid manure
Dairy2017 <- read.csv("input/2017/Dairy2017.csv",header=T)
Pigs2017 <- read.csv("input/2017/Pigs2017.csv",header=T)

#2022 data
Dairy2022 <- read.csv("input/2022/Dairy2022.csv",header=T)
Pigs2022 <- read.csv("input/2022/Pigs2022.csv",header=T)

#Create a list for the data
LMS4_list <- list(Dairy2017,Dairy2022,Pigs2017,Pigs2022)
prov.name <- c(sort(unique(Dairy2017$prov)),"SK","National")


#A.empty Dairy table
df.LMS4_D <- data.frame(Livestock = rep("Dairy",each = 9),
                        prov = prov.name,
                        Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
                        )
#B.empty Pigs table
df.LMS4_P <- data.frame(Livestock = rep("Pigs",each = 9),
                        prov = prov.name,
                        Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
                        )

#obtain the # farms 
#For Dairy
for (j in 1:2) {
  for (i in 1:8) {
    df.LMS4_D[i,j+2] <- sum(LMS4_list[[j]]$LMS4 > 0 & 
                              LMS4_list[[j]]$prov == df.LMS4_D$prov[i],
                            na.rm = T)
  }
}
df.LMS4_D[9,3:4] <- colSums(df.LMS4_D[1:8,3:4]) #National number

#For Pigs
for (j in 3:4) {
  for (i in 1:8) {
    df.LMS4_P[i,j] <- sum(LMS4_list[[j]]$LMS4 > 0 & 
                              LMS4_list[[j]]$prov == df.LMS4_P$prov[i],
                            na.rm = T)
  }
}
df.LMS4_P[9,3:4] <- colSums(df.LMS4_P[1:8,3:4]) #National number


#Calculate the weighted results answer by answer
#there are 4 answers with 2 years so 14 columns
#Dairy
for (i in 1:2) {
temp <- aggregate(LMS4_list[[i]]$weight~LMS4+prov,
               data = LMS4_list[[i]], sum)
#merge the weighted value to the table
df.LMS4_D <- left_join(df.LMS4_D,
                       reshape(temp, idvar = "prov",timevar = "LMS4",direction = "wide" ),
                       by="prov")
}

df.LMS4_D[is.na(df.LMS4_D)] <- 0
#round the number
df.LMS4_D[,5:ncol(df.LMS4_D)] <- 
               round(df.LMS4_D[,5:ncol(df.LMS4_D)],1)
#national number
df.LMS4_D[9,5:ncol(df.LMS4_D)] <- 
  colSums(df.LMS4_D[1:8,5:ncol(df.LMS4_D)])

#Pigs
for (i in 3:4) {
  temp <- aggregate(LMS4_list[[i]]$weight~LMS4+prov,
                    data = LMS4_list[[i]], sum)
  #merge the weighted value to the table
  df.LMS4_P <- left_join(df.LMS4_P,
                         reshape(temp, idvar = "prov",timevar = "LMS4",direction = "wide" ),
                         by="prov")
}

df.LMS4_P[is.na(df.LMS4_P)] <- 0
#round the number
df.LMS4_P[,5:ncol(df.LMS4_P)] <- 
  round(df.LMS4_P[,5:ncol(df.LMS4_P)],1)
#national number
df.LMS4_P[9,5:ncol(df.LMS4_P)] <- 
  colSums(df.LMS4_P[1:8,5:ncol(df.LMS4_P)])

#reorder the columns by column names
df.LMS4_Dw <- cbind(df.LMS4_D[,1:4],
                   df.LMS4_D[,sort(colnames(df.LMS4_D)[5:ncol(df.LMS4_D)])])
df.LMS4_Pw <- cbind(df.LMS4_P[,1:4],
                   df.LMS4_P[,sort(colnames(df.LMS4_P)[5:ncol(df.LMS4_P)])])

#add year
year <- c("","",rep(c("2017","2022"),5))
df.LMS4_Dw <- rbind(year,df.LMS4_Dw)
df.LMS4_Pw <- rbind(year,df.LMS4_Pw)

#make a list for weighted results
LMS4weight_list <- list(df.LMS4_Dw,df.LMS4_Pw)

#change column names
for (i in 1:2) {
    colnames(LMS4weight_list[[i]]) <- 
    c("Livestock","Province",
      "Farm response","",
      "< 3 Months","",
      "3-5 Months","",
      "5-12 Months","",
      "> 1 Year","")
  }

LMS4weight <- rbind(as.data.frame(LMS4weight_list[[1]]),
                    as.data.frame(LMS4weight_list[[2]]))

#Export tables to excel
write.xlsx(LMS4weight,
             file = "results/LMS4_storage time.xlsx",
             sheetName = "weighted values by province", 
             row.names = F, col.names = T, append = TRUE)


#calculate the weighted % results
df.LMS4_D[,5:8] <- round(proportions(
                           as.matrix(df.LMS4_D[,5:8]),1)*100,1)
df.LMS4_D[,9:12] <- round(proportions(
                          as.matrix(df.LMS4_D[,9:12]),1)*100,1)
df.LMS4_P[,5:8] <- round(proportions(
                          as.matrix(df.LMS4_P[,5:8]),1)*100,1)
df.LMS4_P[,9:12] <- round(proportions(
                          as.matrix(df.LMS4_P[,9:12]),1)*100,1)

#aggregate time < 5 months, which are column 5 and 7 (2017) and column 6 and 8 (2022)
#Not a good method, but very simple
df.LMS4_D[1:9,5] <- as.numeric(df.LMS4_D[1:9,5]) + as.numeric(df.LMS4_D[1:9,6]) #2017
df.LMS4_D[1:9,9] <- as.numeric(df.LMS4_D[1:9,9]) + as.numeric(df.LMS4_D[1:9,10]) #2022
df.LMS4_D <-df.LMS4_D[,c(-6,-10)] 

#aggregate time < 5 months, which are column 5 and 7 (2017) and column 6 and 8 (2022)
df.LMS4_P[1:9,5] <- as.numeric(df.LMS4_P[1:9,5]) + as.numeric(df.LMS4_P[1:9,8]) #2017
df.LMS4_P[1:9,9] <- as.numeric(df.LMS4_P[1:9,9]) + as.numeric(df.LMS4_P[1:9,12]) #2022
df.LMS4_P <-df.LMS4_P[,c(-8,-12)]

#replace NaN to 0
df.LMS4_D[is.na(df.LMS4_D)] <- 0
#replace NaN to 0
df.LMS4_P[is.na(df.LMS4_P)] <- 0

#reorder the columns by column names
df.LMS4_D <- cbind(df.LMS4_D[,1:4],
                   df.LMS4_D[,sort(colnames(df.LMS4_D)[5:ncol(df.LMS4_D)])])
df.LMS4_P <- cbind(df.LMS4_P[,1:4],
                   df.LMS4_P[,sort(colnames(df.LMS4_P)[5:ncol(df.LMS4_P)])])

#add year
year <- c("","",rep(c("2017","2022"),5))
df.LMS4_D <- rbind(year,df.LMS4_D)
df.LMS4_P <- rbind(year,df.LMS4_P)

LMS4wp_list <- list(df.LMS4_D,df.LMS4_P)
#change column names
for (i in 1:2) {
  colnames(LMS4wp_list[[i]]) <- 
    c("Livestock","Province",
      "Farm response","",
      "< 5 Months","",
      "5-12 Months","",
      "> 1 Year","")
}


LMS4wp <- rbind(as.data.frame(LMS4wp_list[[1]]),
                    as.data.frame(LMS4wp_list[[2]]))

#write the result
write.xlsx(LMS4wp,
             file = "results/LMS4_storage time.xlsx",
             sheetName = "wetight percentage by province",
           row.names = F, col.names = T, append = TRUE)
