library(data.table)
library(tidyverse)


plan <- fread('VirovorySupplies_20260127.csv')

plan$totalVol <- (plan$Reps*plan$`VolumeAdded_Virus_DOM_ASW)` *2)

volumes <- plan %>% 
  group_by(., Treatment) %>% 
  summarize(., totalVirus_ml=sum(totalVol))
  