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

#Keep animal, province and weight and manure management type
Table3_2017 <- Livestock_2017 %>%
  select(Livestock = SectorID, Prov = PROV, Weight = WGTLIVE_AAFC, MTS2 = MTS2) %>%
  mutate(MTS2= case_when( 
          MTS2 == 1 ~ "Liquid",
          MTS2 == 2 ~ "Solid",
          MTS2 == 3 ~ "Both"))
  group_by(Prov,Livestock,MTS2)%>%
  summarise(weight.num = round(sum(Weight),1)) %>%
  na.omit()

Beef_2017 <- Table3_2017 %>%
  filter(Livestock == "Beef") %>%
  #keep the order identical to Table 3       
  MTS2 = factor(MTS2, levels = c("Liquid","Solid", "Both")) %>%                    
  mutate(Prov = factor(Prov, levels = rev(c("NS","NB","QC","ON","MB","SK","AB","BC")))) %>%
    pivot_wider(
  names_from = MTS2,
  values_from = weight.num
  )







Table3_2022 <- Livestock_2022 %>%
  select(Livestock = FarmType, Prov = prov, Weight = DWEIGHT_LIVE_FINAL, MTS2 = MTS02) %>%
  mutate(MTS2= case_when( 
    MTS2 == 1 ~ "Liquid",
    MTS2 == 2 ~ "Solid",
    MTS2 == 3 ~ "Both"),
    #keep the order identical to Table 3       
    MTS2 = factor(MTS2, levels = c("Liquid","Solid", "Both"))) %>%                    
    mutate(Prov = factor(Prov, levels = rev(c("NS","NB","QC","ON","MB","SK","AB","BC"))))
  


x<-table(Table3_2017$MTS2,  Table3_2017$Prov, Table3_2017$Livestock)

table(Table2_2022$MTS02, Table2_2022$FarmType, Table2_2022$prov)  


  
total_table <- aggregate(DWEIGHT_LIVE_FINAL ~ prov + FarmType + MTS02, data = Table2_2022, sum)


total_table <- aggregate(DWEIGHT_LIVE_FINAL ~ prov + F + MTS02, data = Table2_2022, sum)
