#LMS2 is a question regarding storage type
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
LMS2_list <- list(Dairy2017,Dairy2022,Pigs2017,Pigs2022)
prov.name <- c(sort(unique(Dairy2017$prov)),"SK","National")




#A.empty Dairy table
df.LMS2_D <- data.frame(Livestock = rep("Dairy",each = 9),
                        prov = prov.name,
                        Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
)
#B.empty Pigs table
df.LMS2_P <- data.frame(Livestock = rep("Pigs",each = 9),
                        prov = prov.name,
                        Farm.2017 = rep(0,9), Farm.2022 = rep(0,9)
)


#obtain the # farms 
#For Dairy
for (j in 1:2) {
  for (i in 1:8) {
    df.LMS2_D[i,j+2] <- sum(LMS2_list[[j]]$LMS2 > 0 & 
                              LMS2_list[[j]]$prov == df.LMS2_D$prov[i],
                            na.rm = T)
  }
}
df.LMS2_D[9,3:4] <- colSums(df.LMS2_D[1:8,3:4]) #National number

#For Pigs
for (j in 3:4) {
  for (i in 1:8) {
    df.LMS2_P[i,j] <- sum(LMS2_list[[j]]$LMS2 > 0 & 
                              LMS2_list[[j]]$prov == df.LMS2_P$prov[i],
                            na.rm = T)
  }
}
df.LMS2_P[9,3:4] <- colSums(df.LMS2_P[1:8,3:4]) #National number


#Calculate the weighted results answer by answer
#there are 7 answers with 2 years so 14 columns
#Dairy
for (i in 1:2) {
temp <- aggregate(LMS2_list[[i]]$weight~LMS2+prov,
               data = LMS2_list[[i]], sum)
#merge the weighted value to the table
df.LMS2_D <- left_join(df.LMS2_D,
                       reshape(temp, idvar = "prov",timevar = "LMS2",direction = "wide" ),
                       by="prov")
}

df.LMS2_D[is.na(df.LMS2_D)] <- 0
#round the number
df.LMS2_D[,5:ncol(df.LMS2_D)] <- 
               round(df.LMS2_D[,5:ncol(df.LMS2_D)],1)
#national number
df.LMS2_D[9,5:ncol(df.LMS2_D)] <- 
  colSums(df.LMS2_D[1:8,5:ncol(df.LMS2_D)])

#Pigs
for (i in 3:4) {
  temp <- aggregate(LMS2_list[[i]]$weight~LMS2+prov,
                    data = LMS2_list[[i]], sum)
  #merge the weighted value to the table
  df.LMS2_P <- left_join(df.LMS2_P,
                         reshape(temp, idvar = "prov",timevar = "LMS2",direction = "wide" ),
                         by="prov")
}

df.LMS2_P[is.na(df.LMS2_P)] <- 0
#round the number
df.LMS2_P[,5:ncol(df.LMS2_P)] <- 
  round(df.LMS2_P[,5:ncol(df.LMS2_P)],1)
#national number
df.LMS2_P[9,5:ncol(df.LMS2_P)] <- 
  colSums(df.LMS2_P[1:8,5:ncol(df.LMS2_P)])
#Because no response in Pigs for other in 2022
df.LMS2_P[,18] <- 0


#reorder the columns by column names
df.LMS2_Dw <- cbind(df.LMS2_D[,1:4],
                   df.LMS2_D[,sort(colnames(df.LMS2_D)[5:ncol(df.LMS2_D)])])
df.LMS2_Pw <- cbind(df.LMS2_P[,1:4],
                   df.LMS2_P[,sort(colnames(df.LMS2_P)[5:ncol(df.LMS2_P)])])

#add year
year <- c("","",rep(c("2017","2022"),8))
df.LMS2_Dw <- rbind(year,df.LMS2_Dw)
df.LMS2_Pw <- rbind(year,df.LMS2_Pw)

#make a list for weighted results
LMS2weight_list <- list(df.LMS2_Dw,df.LMS2_Pw)

#change column names
for (i in 1:2) {
    colnames(LMS2weight_list[[i]]) <- 
    c("Livestock","Province",
      "Farm response","",
      "Multi-Cell Below Ground Pit","",
      "Earthen Lagoon","",
      "Other Below-Ground Tank","",
      "Above-Ground Tank Outside","",
      "Partial Below Ground Tank Outside","",
      "Pit ot Tank Below Floor in Building","",

      "Other","")
}

LMS2weight <- rbind(as.data.frame(LMS2weight_list[[1]]),
                    as.data.frame(LMS2weight_list[[2]]))

#Export tables to excel
write.xlsx(LMS2weight,
             file = "results/LMS2_storage type.xlsx",
             sheetName = "weighted values by province", 
             row.names = F, col.names = T, append = TRUE)


#calculate the weighted % results
df.LMS2_D[,5:11] <- round(proportions(
                           as.matrix(df.LMS2_D[,5:11]),1)*100,1)
df.LMS2_D[,12:18] <- round(proportions(
                          as.matrix(df.LMS2_D[,12:18]),1)*100,1)
df.LMS2_P[,5:11] <- round(proportions(
                          as.matrix(df.LMS2_P[,5:11]),1)*100,1)
df.LMS2_P[,12:17] <- round(proportions(
                          as.matrix(df.LMS2_P[,12:17]),1)*100,1)
#replace NaN to 0
df.LMS2_D[is.na(df.LMS2_D)] <- 0
#replace NaN to 0
df.LMS2_P[is.na(df.LMS2_P)] <- 0

#reorder the columns by column names
df.LMS2_D <- cbind(df.LMS2_D[,1:4],
                   df.LMS2_D[,sort(colnames(df.LMS2_D)[5:ncol(df.LMS2_D)])])
df.LMS2_P <- cbind(df.LMS2_P[,1:4],
                   df.LMS2_P[,sort(colnames(df.LMS2_P)[5:ncol(df.LMS2_P)])])

#add year
year <- c("","",rep(c("2017","2022"),8))
df.LMS2_D <- rbind(year,df.LMS2_D)
df.LMS2_P <- rbind(year,df.LMS2_P)

LMS2wp_list <- list(df.LMS2_D,df.LMS2_P)
#change column names
for (i in 1:2) {
  colnames(LMS2wp_list[[i]]) <- 
                          c("Livestock","Province",
                          "Farm response","",
                          "Multi-Cell Below Ground Pit","",
                          "Earthen Lagoon","",
                          "Other Below-Ground Tank","",
                          "Above-Ground Tank Outside","",
                          "Partial Below Ground Tank Outside","",
                          "Pit ot Tank Below Floor in Building","",
                          "Other","")
}


LMS2wp <- rbind(as.data.frame(LMS2wp_list[[1]]),
                    as.data.frame(LMS2wp_list[[2]]))

#write the result
write.xlsx(LMS2wp,
             file = "results/LMS2_storage type.xlsx",
             sheetName = "wetight percentage by province",
           row.names = F, col.names = T, append = TRUE)
