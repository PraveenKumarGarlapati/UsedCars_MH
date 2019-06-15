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

Data_Test%>%colnames()
Data_Test$Price <- NA
Data_Train$status <- "train"
Data_Test$status <- "test"

fulldf <- bind_rows(Data_Train, Data_Test)

name_spl <- str_split_fixed(fulldf$Name, " ", 10)
name_spl <- as.data.frame(name_spl)

fulldf <- bind_cols(fulldf, name_spl)

fulldf%>%
  mutate_at(vars(V1,V2), tolower)%>%
  count(V1,V2, status)%>%
  dcast(V1+V2 ~ status, value.var = "n")%>%
  arrange(V1)

#Change BMW Model names, add an X to it
#Maruthi S?
#Isuzu Mu, Mu-X??
#Merge Honds Wr-V and Wrv
#Hidustan and OpelCorsa??

fulldf%>%
  filter(grepl("Cr", New_Price))%>%
  select(Name, New_Price)

fulldf%>%
  arrange(desc(New_Price))%>%
  select(Name, New_Price)



name_spl_t <- str_split_fixed(Data_Test$Name, " ", 10)
name_spl_t <- as.data.frame(name_spl_t)
name_spl_t%>%
  count(V1, sort = T)%>%print(n=50)

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


#//
  
#Analyzing data w.r.t Fuel_Type

Data_Train%>%
  group_by(Fuel_Type)%>%
  summarise(count=n())

c=Data_Train%>%
  filter(Fuel_Type %in% "Electric")

fulldf$Price

fulldf%>%
  group_by(V1, Fuel_Type)%>%
  summarise(meanp = mean(Price, na.rm = TRUE))%>%
  dcast(V1 ~ Fuel_Type, value.var = "meanp")


fulldf <- fulldf%>%
  mutate(Mileage_new = as.numeric(str_extract(Mileage, "[[:digit:]]+")))%>%
  mutate(cr_lk = ifelse(grepl("Cr",New_Price), "Cr", "Lakh"))%>%
  mutate(newcarprice = as.numeric(str_extract(New_Price, "[[:digit:]]+")))%>%
  mutate(newcarprice_1 = ifelse(cr_lk == "Cr", newcarprice*100, newcarprice))%>%
  mutate(engine_new = as.numeric(str_extract(Engine, "[[:digit:]]+")))%>%
  mutate(Power_new = as.numeric(str_extract(Power, "[[:digit:]]+")))%>%
  mutate(age = 2019-Year)
  
#fulldf%>%write_xlsx("tr+test_transformed.xlsx")
fulldf%>%glimpse()

fulldf%>%
  count(V1, V2)

#Average new car price Company-Model-wise
fulldf%>%
  distinct(V1, V2, newcarprice_1)%>%
  arrange(V1, V2)%>%
  group_by(V1,V2)%>%
  summarise(avgnewpriceV1V2 = mean(newcarprice_1, na.rm = TRUE))->new1

fulldf%>%dim()
fulldf <- fulldf%>%
  left_join(new1, by = c("V1","V2"))

train_t <- fulldf%>%
  filter(status == "train")%>%
  select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Price)

train_t%>%glimpse()
train_t$Owner_Type <- as.factor(train_t$Owner_Type)
train_t$Mileage_new[is.na(train_t$Mileage_new)] = mean(train_t$Mileage_new, na.rm = TRUE)

test_t <- fulldf%>%
  filter(status == "test")%>%
  select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Price)

test_t$Owner_Type <- as.factor(test_t$Owner_Type)
#Treating for the Not-Found in train rows
test_t%>%
  filter(grepl("Maruti",V1))%>%
  count(V2)%>%print(n=50)

test_t$V2[test_t$V2 == "Motors"] = "800"
test_t$V2[test_t$V2 == "1.4Gsi"] = "800"
###

dim(train_t)
train_t%>%glimpse()
test_t%>%glimpse()

colSums(is.na(train_t))

lm = lm(Price ~ ., train_t[ ,-c(4,6)])
lm_pred <- predict(lm, test_t)

lm_pred <- lm_pred%>%as.data.frame()
colnames(lm_pred) <- "Price"

write_xlsx(lm_pred, "lm_pred.xlsx")
