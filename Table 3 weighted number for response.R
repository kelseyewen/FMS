#To calculate Table 3. Number of farms of each livestock type reporting...
library(tidyverse);library(xlsx);library(dplyr)
#Separate All Livestock Table Into Each Livestock Type
Livestock_2017 <- read.csv("input/2017/2017livestock.csv",header = T)
Livestock_2022 <- read.csv("input/2022/2022livestock.csv",header = T)

#Find the provinces, StatCan SGC code to Int approved alpha code
#Only 2022 since 2017 was replaced in Excel
prov.code <- data.frame(SGC=c(12,13,24,35,46,47,48,59),
                        alphacode=c("NS","NB","QC","ON","MB","SK","AB","BC"))
for (i in 1:nrow(prov.code)){
  Livestock_2022$prov <- replace(Livestock_2022$prov,
                                 Livestock_2022$prov == prov.code[i,1],
                                 prov.code[i,2])
}

#Create table for all possible combination of prov, livestock, MTS2
all_combinations <- expand.grid(
  Prov = c("NS","NB","QC","ON","MB","SK","AB","BC"),
  Livestock = c("Beef","Dairy","Pigs","Poultry"),
  MTS2 = c("Liquid","Solid","Both")
  )


#Keep animal, province and weight and manure management type
Table3_2017 <- Livestock_2017 %>%
  select(Livestock = SectorID, Prov = PROV, Weight = WGTLIVE_AAFC, MTS2 = MTS2) %>%
  mutate(MTS2= case_when( 
          MTS2 == 1 ~ "Liquid",
          MTS2 == 2 ~ "Solid",
          MTS2 == 3 ~ "Both")) %>%
  group_by(Prov,Livestock,MTS2)%>%
  summarise(weight.num = round(sum(Weight),1)) %>%
  na.omit()

#merge with all combination and do the weighted calculation including check NA data 
Table3_2017 <- merge(all_combinations, Table3_2017,
                     by = c("Prov", "Livestock", "MTS2"),
                     all.x = TRUE)

Table3_2017$weight.num <- ave(Table3_2017$weight.num, Table3_2017$Prov, Table3_2017$Livestock,
                                      FUN = function(x) if(all(is.na(x))) NA else replace(x, is.na(x), 0))
#Create new column for pivot wider
Table3_2017$Livestock_MTS <- paste(Table3_2017$Livestock, Table3_2017$MTS2, sep = "_")
#Pivot 
T_2017 <- Table3_2017 %>%
  select(Prov,Livestock_MTS,weight.num) %>%
  pivot_wider(
    names_from = Livestock_MTS,
    values_from = weight.num) %>%
  mutate(Prov = factor(Prov, levels = rev(c("NS","NB","QC","ON","MB","SK","AB","BC")))) %>%   #keep the order identical to Table 3  
  arrange(Prov) %>%
  select(-c(8:10),everything(),c(8:10)) #not a clean method, but works

#2022

Table3_2022 <- Livestock_2022 %>%
  select(Livestock = FarmType, Prov = prov, Weight = DWEIGHT_LIVE_FINAL, MTS2 = MTS02) %>%
  mutate(MTS2= case_when( 
    MTS2 == 1 ~ "Liquid",
    MTS2 == 2 ~ "Solid",
    MTS2 == 3 ~ "Both")) %>%
    group_by(Prov,Livestock,MTS2)%>%
      summarise(weight.num = round(sum(Weight),1)) %>%
      na.omit()
    
#merge with all combination and do the weighted calculation including check NA data 
Table3_2022 <- merge(all_combinations, Table3_2022,
                     by = c("Prov", "Livestock", "MTS2"),
                     all.x = TRUE)

Table3_2022$weight.num <- ave(Table3_2022$weight.num, Table3_2022$Prov, Table3_2022$Livestock,
                              FUN = function(x) if(all(is.na(x))) NA else replace(x, is.na(x), 0))
#Create new column for pivot wider
Table3_2022$Livestock_MTS <- paste(Table3_2022$Livestock, Table3_2022$MTS2, sep = "_")
#Pivot 
T_2022 <- Table3_2022 %>%
  select(Prov,Livestock_MTS,weight.num) %>%
  pivot_wider(
    names_from = Livestock_MTS,
    values_from = weight.num) %>%
  mutate(Prov = factor(Prov, levels = rev(c("NS","NB","QC","ON","MB","SK","AB","BC")))) %>%   #keep the order identical to Table 3  
  arrange(Prov) %>%
  select(-c(8:10),everything(),c(8:10)) #not a clean method, but works
  
  

#Output the files
write.xlsx(T_2017,
           file = "results/Table 3 number of response.xlsx",
           sheetName = "2017",col.names = T)
write.xlsx(T_2022,
           file = "results/Table 3 number of response.xlsx",
           sheetName = "2022",
           col.names = T, append = TRUE)
