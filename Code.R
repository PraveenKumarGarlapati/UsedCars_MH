#Used Cars

library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(reshape2)
library(caret)
library(randomForest)
library(xgboost)

# ImportingData -----------------------------------------------------------

dim(Data_Train)
dim(Data_Test)
Sample_submission

Data_Test%>%
  filter(!Name %in% Data_Train$Name)%>%
  distinct(Name)

Data_Train%>%
  count(Name)%>%
  sample_n(20)

Data_Train

  
name_spl <- str_split_fixed(Data_Train$Name, " ", 10)
name_spl <- as.data.frame(name_spl)
name_spl%>%
  count(V1, sort = T) %>% print(n=50)



name_spl_t <- str_split_fixed(Data_Test$Name, " ", 10)
name_spl_t <- as.data.frame(name_spl_t)
name_spl_t%>%
  count(V1, sort = T)

name_spl_t%>%
  filter(!V1 %in% name_spl$V1)

name_spl%>%
  filter(V1 %in% c("Hindu","Ope"))

Data_Test%>%
  mutate


Data_Train%>%
  group_by(Name)%>%
  summarise(m = mean(Price))%>%
  arrange(m)




