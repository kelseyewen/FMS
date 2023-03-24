#SMS5 is a question regarding storage time
# Load the required library
library(tidyverse);library(xlsx)

#2017 data
Dairy2017 <- read.csv("input/2017/Dairy2017.csv",header=T)
Beef2017 <- read.csv("input/2017/Beef2017.csv",header=T)
Poultry2017 <- read.csv("input/2017/Poultry2017.csv",header=T)

#2022 data
Dairy2022 <- read.csv("input/2022/Dairy2022.csv",header=T)
Beef2022 <- read.csv("input/2022/Beef2022.csv",header=T)
Poultry2022 <- read.csv("input/2022/Poultry2022.csv",header=T)

SMS5_list <- list(Dairy2017,Dairy2022,
                  Beef2017,Beef2022,
                  Poultry2017,Poultry2022)

#A.
#National table, SMS5*.w = weight,  SNS5*.n = # farms
df.SMS5 <- data.frame(Livestock = rep(c("Dairy","Beef","Poultry"),each = 2),
                      Year = rep(c("2017","2022"),3),
                      SMS5a.w = rep(0,6), SMS5b.w = rep(0,6), #<6 and 6-12 m
                      SMS5c.w = rep(0,6), SMS5d.w = rep(0,6), #1-2 yr and >2 yr
                      SMS5a.n = rep(0,6), SMS5b.n = rep(0,6),
                      SMS5c.n = rep(0,6), SMS5d.n = rep(0,6) 
                      )
#B.
#National table, SM54*.wp = weight%,  SNS5*.n = # farms
df.SMS5wp <- data.frame(Livestock = rep(c("Dairy","Beef","Poultry"),each = 2),
                      Year = rep(c("2017","2022"),3),
                      SMS5a.wp = rep(0,6), SMS5a.n = rep(0,6),#<6 
                      SMS5b.wp = rep(0,6), SMS5b.n = rep(0,6),#6-12 m
                      SMS5c.wp = rep(0,6), SMS5c.n = rep(0,6),#1-2 yr
                      SMS5d.wp = rep(0,6), SMS5d.n = rep(0,6) #>2y
)


#obtain the # farms for weight table
for (i in 1:6) {
  if(length(count(SMS5_list[[i]],SMS5)[,2]) > 4) {
    df.SMS5[i,7:10] <- count(SMS5_list[[i]],SMS5)[,2][1:4]
  } else {
    df.SMS5[i,7:10] <- c(count(SMS5_list[[i]],SMS5)[,2][1:3],0)
  }
}

#obtain the # farms for weight percentage table
for (i in 1:6) {
  if(length(count(SMS5_list[[i]],SMS5)[,2]) > 4) {
    df.SMS5wp[i,7:10] <- count(SMS5_list[[i]],SMS5)[,2][1:4]
  } else {
    df.SMS5wp[i,7:10] <- c(count(SMS5_list[[i]],SMS5)[,2][1:3],0)
  }
}


#obtain the weight result for weight table
for (i in 1:6) {
  if (length(count(SMS5_list[[i]],SMS5)[,2]) > 4) {
    df.SMS5[i,3:6] <- 
      aggregate(weight~SMS5, data = SMS5_list[[i]], sum) [,2][1:4]
    } else {
    df.SMS5[i,3:6] <- 
      c(aggregate(weight~SMS5, data = SMS5_list[[i]], sum) [,2][1:3],0)
  }
}
df.SMS5[,3:6] <- round(df.SMS5[,3:6],1)

#obtain the weight percentage result for weight percentage table
df.SMS5wp[,3:6] <- round(proportions(as.matrix(df.SMS5[,3:6]),1)*100,1)
  


#Insert a new row for responses name
df.SMS5 <- rbind(c("Livestock","Year",rep(c("weight","farm #")
                                          ,each = 4)),df.SMS5)
df.SMS5 <- rbind(c("","",rep(c("<6 months",
                               "6-12 months",
                               "1-2 Years", 
                               "> 2 Years"),each =2)),df.SMS5)
df.SMS5wp <- rbind(c("Livestock","Year",rep(c("weight","farm #")
                                            ,each = 4)),df.SMS5wp)
df.SMS5wp <- rbind(c("","",rep(c("<6 months",
                               "6-12 months",
                               "1-2 Years", 
                               "> 2 Years"),each =2)),df.SMS5wp)


#Export tables to excel
xlsx.list <- list(SMS5.weight = df.SMS5, SMS5.weightpercent = df.SMS5wp)
for (i in 1:2) {
  write.xlsx(xlsx.list[i],
             file = "results/SMS5_storage time.xlsx",
             sheetName = names(xlsx.list)[i],
             row.names = F, col.names= F, append = TRUE)}
