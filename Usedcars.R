#UsedCars

library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(reshape2)
library(caret)
library(randomForest)
library(xgboost)
library(party)

# ImportingData -----------------------------------------------------------
Data_Train <- read_xlsx("C:/Users/u278677/Downloads/Data_Train.xlsx")
Data_Test <- read_xlsx("C:/Users/u278677/Downloads/Data_Test.xlsx")

dim(Data_Train)
dim(Data_Test)
Sample_submission

# Data_Test%>%
#   filter(!Name %in% Data_Train$Name)%>%
#   distinct(Name)
# 
# Data_Train%>%
#   count(Name)%>%
#   sample_n(20)

# Data_Test%>%colnames()
Data_Test$Price <- NA
Data_Train$status <- "train"
Data_Test$status <- "test"

fulldf <- bind_rows(Data_Train, Data_Test)

name_spl <- str_split_fixed(fulldf$Name, " ", 10)
name_spl <- as.data.frame(name_spl)
fulldf <- bind_cols(fulldf, name_spl)

# fulldf%>%
#   mutate_at(vars(V1,V2), tolower)%>%
#   count(V1,V2, status)%>%
#   dcast(V1+V2 ~ status, value.var = "n")%>%
#   arrange(V1)

#Change BMW Model names, add an X to it
#Maruthi S?
#Isuzu Mu, Mu-X??
#Merge Honds Wr-V and Wrv
#Hidustan and OpelCorsa??
# 
# fulldf%>%
#   filter(grepl("Cr", New_Price))%>%
#   select(Name, New_Price)
# 
# fulldf%>%
#   arrange(desc(New_Price))%>%
#   select(Name, New_Price)

# name_spl_t <- str_split_fixed(Data_Test$Name, " ", 10)
# name_spl_t <- as.data.frame(name_spl_t)
# name_spl_t%>%
#   count(V1, sort = T)%>%print(n=50)
# 
# name_spl_t%>%
#   filter(!V1 %in% name_spl$V1)
# 
# name_spl%>%
#   filter(V1 %in% c("Hindu","Ope"))
# 
# Data_Train%>%
#   group_by(Name)%>%
#   summarise(m = mean(Price))%>%
#   arrange(m)


#//

#Analyzing data w.r.t Fuel_Type

# Data_Train%>%
#   group_by(Fuel_Type)%>%
#   summarise(count=n())
# 
# c=Data_Train%>%
#   filter(Fuel_Type %in% "Electric")
# 
# fulldf$Price
# 
# fulldf%>%
#   group_by(V1, Fuel_Type)%>%
#   summarise(meanp = mean(Price, na.rm = TRUE))%>%
#   dcast(V1 ~ Fuel_Type, value.var = "meanp")
# 

fulldf <- fulldf%>%
  mutate(Mileage_new = as.numeric(str_extract(Mileage, "[[:digit:]]+")))%>%
  mutate(cr_lk = ifelse(grepl("Cr",New_Price), "Cr", "Lakh"))%>%
  mutate(newcarprice = as.numeric(str_extract(New_Price, "[[:digit:]]+")))%>%
  mutate(newcarprice_1 = ifelse(cr_lk == "Cr", newcarprice*100, newcarprice))%>%
  mutate(engine_new = as.numeric(str_extract(Engine, "[[:digit:]]+")))%>%
  mutate(Power_new = as.numeric(str_extract(Power, "[[:digit:]]+")))%>%
  mutate(age = 2019-Year)

#fulldf%>%write_xlsx("tr+test_transformed.xlsx")
# fulldf%>%glimpse()
# 
# fulldf%>%
#   count(V1, V2)
 
#Average new car price Company-Model-wise
fulldf%>%
  distinct(V1, V2, newcarprice_1)%>%
  arrange(V1, V2)%>%
  group_by(V1,V2)%>%
  summarise(avgnewpriceV1V2 = mean(newcarprice_1, na.rm = TRUE))->new1

fulldf%>%dim()
fulldf <- fulldf%>%
  left_join(new1, by = c("V1","V2"))


fulldf$V2[fulldf$V2 == "Flying"] = "Continental"
fulldf$V2[fulldf$V2 == "Land"] = "Fortuner"
fulldf$V2[fulldf$V2 == "MU"] = "MUX"
fulldf$V2[fulldf$V2 == "370Z"] = "Terrano"
fulldf$V2[fulldf$V2 == "Abarth"] = "Punto"

fulldf$V2[fulldf$V2 == "Motors"] = "800"
fulldf$V2[fulldf$V2 == "1.4Gsi"] = "800"

fulldf$V1[fulldf$V1 == "Hindustan"] = "Maruti"
fulldf$V1[fulldf$V1 == "OpelCorsa"] = "Maruti"



# train_t <- fulldf%>%
#   filter(status == "train")%>%
#   select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Price)
# 
# train_t%>%glimpse()
# train_t$Owner_Type <- as.factor(train_t$Owner_Type)
# train_t$Mileage_new[is.na(train_t$Mileage_new)] = mean(train_t$Mileage_new, na.rm = TRUE)
# 
# test_t <- fulldf%>%
#   filter(status == "test")%>%
#   select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Price)
# 
# test_t$Owner_Type <- as.factor(test_t$Owner_Type)
# #Treating for the Not-Found in train rows
# test_t%>%
#   filter(grepl("Maruti",V1))%>%
#   count(V2)%>%print(n=50)
# 
# test_t$V2[test_t$V2 == "Motors"] = "800"
# test_t$V2[test_t$V2 == "1.4Gsi"] = "800"
###
# 
# dim(train_t)
# train_t%>%glimpse()
# test_t%>%glimpse()
# 
# colSums(is.na(train_t))
# 
# lm = lm(Price ~ ., train_t[ ,-c(4,6)])
# lm_pred <- predict(lm, test_t)
# 
# lm_pred <- lm_pred%>%as.data.frame()
# colnames(lm_pred) <- "Price"
# 
# lm_pred <- lm_pred%>%mutate(Price = abs(Price))
# write_xlsx(lm_pred, "lm_pred.xlsx")
# 
#######
#Replacing all the Nonmatching V2 Values as well with relavant ones
# Data_Test%>%
#   filter(!Name %in% Data_Train$Name)
# 
# test_t%>%
#   filter(!V2 %in% train_t$V2)
# 
# test_t$V2[test_t$V2 == "Flying"] = "Continental"
# test_t$V2[test_t$V2 == "Land"] = "Fortuner"
# test_t$V2[test_t$V2 == "MU"] = "MUX"
# test_t$V2[test_t$V2 == "370Z"] = "Terrano"
# test_t$V2[test_t$V2 == "Abarth"] = "Punto"
# 
# test_t$V2[test_t$V2 == "Motors"] = "800"
# test_t$V2[test_t$V2 == "1.4Gsi"] = "800"
# 
# test_t$V1[test_t$V1 == "Hindustan"] = "Maruti"
# test_t$V1[test_t$V1 == "OpelCorsa"] = "Maruti"
# 
# 
# 
# lm = lm(Price ~ ., train_t[ ,-c(5,6)])
# lm_pred <- predict(lm, test_t)
# 
# lm_pred <- lm_pred%>%as.data.frame()
# colnames(lm_pred) <- "Price"

# #Took absolute values of predictions
# write_xlsx(lm_pred, "lm_pred_w_V2.xlsx")
# 
# #DT
# dt = ctree(Price ~ ., train_t[ ,-6])
# dt_pred <- predict(dt, test_t)
# 
# dt_pred <- as.data.frame(dt_pred)
# colnames(dt_pred) <- "Price"
# 
# write_xlsx(dt_pred, "dt_pred_.xlsx")

##Adding price of fuel and getting running cost per kilometre

# fulldf$rateperunit[is.na(fulldf$rateperunit)] = 2
# fulldf$rateperunit[which(fulldf$rateperunit==Inf)] = NA
# 
# #Removing Inf values in rateperunit col
# fulldf%>%
#   filter(is.na(rateperunit))%>%
#   count(Fuel_Type)
# 
# fulldf%>%
#   group_by(Fuel_Type)%>%
#   summarise(mean(rateperunit,na.rm = T))
# 
# fulldf$rateperunit[is.na(fulldf$rateperunit)] = 4
# 
####


# 
# train_t <- fulldf%>%
#   filter(status == "train")%>%
#   select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Fuel_Type, rateperunit, Price)
# train_t%>%glimpse()
# 
# train_t$Owner_Type <- as.factor(train_t$Owner_Type)
# train_t$Mileage_new[is.na(train_t$Mileage_new)] = mean(train_t$Mileage_new, na.rm = TRUE)
# train_t$Fuel_Type <- as.factor(train_t$Fuel_Type)
# 
# test_t <- fulldf%>%
#   filter(status == "test")%>%
#   select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Fuel_Type, rateperunit, Price)
# test_t%>%glimpse()
# 
# test_t$Owner_Type <- as.factor(test_t$Owner_Type)
# test_t$Mileage_new[is.na(test_t$Mileage_new)] = mean(test_t$Mileage_new, na.rm = TRUE)
# test_t$Fuel_Type <- as.factor(test_t$Fuel_Type)
# 
# # fulldf$rateperunit[which(fulldf$rateperunit==Inf)] = NA
# # test_t$rateperunit[which(test_t$rateperunit==Inf)] = NA
# 
# #LM
# lm = lm(Price ~ . , train_t[ ,-c(5,6)])
# lm_pred <- predict(lmodel, test)
# 
# lm_pred <- lm_pred%>%as.data.frame()
# colnames(lm_pred) <- "Price"
# 
# #Took absolute values of predictions
# write_xlsx(lm_pred, "lm_pred_all.xlsx")
# 
# 
# #DT
# dt = ctree(Price ~ ., train_t[ ,-c(5,6)])
# dt_pred <- predict(dt, test_t)
# 
# dt_pred <- as.data.frame(dt_pred)
# colnames(dt_pred) <- "Price"
# 
# write_xlsx(dt_pred, "dt_pred_.xlsx")
# 
# #Not able to run this dtree
# 
# library(rpart)
# library(rpart.plot)
# dtfit <- rpart(Price~., data = train_t[ ,-c(6)], method = 'anova')
# 
# # test_t_m <- as.matrix(test_t)
# 
# dt_pred <- predict(dtfit, test_t)
# 
# dt_pred <- as.data.frame(dt_pred)
# colnames(dt_pred) <- "Price"
# 
# write_xlsx(dt_pred, "dt_pred_rateperunit_wmil.xlsx")
# 
# 
# ##
# train_t%>%glimpse()
# test_t%>%glimpse()
# test_t%>%
#   filter(Kilometers_Driven %in% train_t$Kilometers_Driven)
# 
#Taking units of Mileage

mileage_brkup = str_split_fixed(fulldf$Mileage, " ",2)
mileage_brkup <- as.data.frame(mileage_brkup)

colnames(mileage_brkup) <- c("M_brkup_1","M_brkup_2")
fulldf <- bind_cols(fulldf, mileage_brkup)

fulldf$M_brkup_11 <- as.numeric(as.character(fulldf$M_brkup_1))
rate <- read_clip_tbl()

fulldf%>%
  filter(Fuel_Type == "Electric")%>%
  select(Mileage, Mileage_new, M_brkup_11)

#Solving mileage = 0 issue
fulldf <- fulldf%>%
  group_by(V1,V2)%>%
  mutate(mil_1 = ifelse(M_brkup_11 == 0, mean(M_brkup_11,na.rm = T), M_brkup_11))%>%
  mutate(mil_1 = ifelse(mil_1 == 0, 10, mil_1))

fulldf <- fulldf%>%
  mutate(rateperunit = Rate/mil_1)

fulldf$rateperunit[is.na(fulldf$rateperunit)] = 2.5
fulldf$mil_1[is.na(fulldf$mil_1)] = 30


# fulldf%>%
#   filter(is.na(Mileage_new))%>%
#   select(Mileage_new, M_brkup_1, M_brkup_2, M_brkup_11,Rate, rateperunit)
# 
#############Fixing mileage = 0 issue
#Adding avg rateperkm of electric to CNG
# 
# fulldf%>%
#   group_by(Fuel_Type)%>%
#   summarise(mean(rateperunit, na.rm = T))
# fulldf%>%
#   group_by(V1,V2)%>%
#   mutate(mil_1 = ifelse(M_brkup_11 == 0, mean(M_brkup_11,na.rm = T), M_brkup_11))%>%
#   filter(mil_1 == 0)%>%
#   select(Mileage, mil_1)%>%print(n=100)

fulldf%>%
  group_by(Location, Fuel_Type)%>%
  summarise(mean(mil_1))%>%
  arrange(Fuel_Type)%>%print(n=100)

fulldf%>%
  filter(Fuel_Type == "CNG")%>%
  select(V1,V2, Fuel_Type, Mileage, mil_1, rateperunit)%>%print(n=100)

# fulldf%>%
#   filter(M_brkup_1 == "0.0")%>%
#   distinct(V1,V2)
# 
# fulldf%>%
#   filter(M_brkup_1 == "0.0")%>%
#   distinct(V1,V2, Fuel_Type)
# 
#--calculate avg mileage of all models
#Not considering fuel since there is just 1 additional model of all 18 such models

fulldf%>%
  write_xlsx("Fulldf_v1_2606.xlsx")

fulldfbck_1 <- fulldf
###############3
#From data Fulldf_v1_2606.xlsx

fulldf <- fulldfbck_1

fulldf_few <- fulldf%>%
  #filter(status == "train")%>%
  select(V1,V2,
         Location,
         Year,
         age,
         Kilometers_Driven,
         Fuel_Type,
         rateperunit,
         Transmission,
         Owner_Type,
         mil_1,
         engine_new,
         Power_new,
         Seats,
         Price)


train <- fulldf%>%
  filter(status == "train")%>%
  select(V1,V2,
         Location,
         Year,
         age,
         Kilometers_Driven,
         Fuel_Type,
         rateperunit,
         Transmission,
         Owner_Type,
         mil_1,
         engine_new,
         Power_new,
         Seats,
         Price)

test <- fulldf%>%
  filter(status == "test")%>%
  select(V1,V2,
         Location,
         Year,
         age,
         Kilometers_Driven,
         Fuel_Type,
         rateperunit,
         Transmission,
         Owner_Type,
         mil_1,
         engine_new,
         Power_new,
         Seats,
         Price)

glimpse(train)
# map(train[ ,c(3,4,7,9,10)], as.factor)%>%as.data.frame()%>%glimpse()
# train[ ,c(3,4,7,9,10)] <- map(train[ ,c(3,4,7,9,10)], as.factor)%>%as.data.frame()
# test[ ,c(3,4,7,9,10)] <- map(test[ ,c(3,4,7,9,10)], as.factor)%>%as.data.frame()
# 
# lmodel <- lm(Price ~ ., data = train)
# lm_pred <- predict(lmodel, test)
# lm_pred <- as.data.frame(lm_pred)
# colnames(lm_pred) <- "Price"
# bind_cols(test, lm_pred)%>%View()
# lm_pred <- lm_pred%>%
#   mutate(Price = abs(Price))
# lm_pred$Price[is.na(lm_pred$Price)] <- mean(lm_pred$Price, na.rm = T)
# 
# colSums(is.na(fulldf))
# colSums(is.na(train))
# 
# fulldf%>%
#   filter(is.na(engine_new1))%>%
#   count(V1,V2)
# 
# fulldf%>%
#   filter(is.na(Power_new1))%>%
#   count(V1,V2)
# 
# fulldf%>%
#   filter(is.na(Seats1))%>%
#   count(V1,V2)
# 
# 
# fulldf <- fulldf%>%
#   group_by(V1,V2)%>%
#   mutate(engine_new1 = ifelse(is.na(engine_new), mean(engine_new, na.rm = T), engine_new))
# 
# fulldf <- fulldf%>%
#   group_by(V1,V2)%>%
#   mutate(Power_new1 = ifelse(is.na(Power_new), mean(Power_new, na.rm = T), Power_new))
# 
# fulldf <- fulldf%>%
#   group_by(V1,V2)%>%
#   mutate(Seats1 = ifelse(is.na(Seats), mean(Seats, na.rm = T), Seats))
# 
# colSums(is.na(fulldf))
# 
# fulldf_bckup <- fulldf
# 
# fulldf$engine_new[is.na(fulldf$engine_new)] <- mean(fulldf$engine_new, na.rm = T)
# fulldf$Power_new[is.na(fulldf$Power_new)] <- mean(fulldf$Power_new, na.rm = T)
# fulldf$rateperunit[is.na(fulldf$rateperunit)] <- mean(fulldf$rateperunit, na.rm = T)
# fulldf$Seats[is.na(fulldf$Seats)] <- mean(fulldf$Seats, na.rm = T)
# fulldf$Mileage_new[is.na(fulldf$Mileage_new)] <- 30
# 
# glimpse(fulldf)
# fulldf[ ,c(1:2,5:10,12,14,26)] <- map(fulldf[ ,c(1:2,5:10,12,14,26)], as.factor)

dt <- ctree(Price ~ ., train)
dt_pred <- predict(dt, test)
dt_pred <- as.data.frame(dt_pred)
colnames(dt_pred) <- "Price"
bind_cols(test, lm_pred)%>%View()
write_xlsx(dt_pred, "dt_all.xlsx")

#Scale
#OneHot Encode 
#RF and Neuralnetwork

glimpse(fulldf)
train[ ,c(3,4,7,9,10)] <- map(train[ ,c(3,4,7,9,10)], as.factor)%>%as.data.frame()
test[ ,c(3,4,7,9,10)] <- map(test[ ,c(3,4,7,9,10)], as.factor)%>%as.data.frame()


a <- dummyVars(" ~ V1+ V2 + Location + Year + Fuel_Type + Transmission + Owner_Type", data = train)
train_0 <- as.data.frame(predict(a, train))

b <- dummyVars(" ~ V1+ V2 + Location + Year + Fuel_Type + Transmission + Owner_Type", data = test)
test_0 <- as.data.frame(predict(b, test))
#Test and train columns not matching
#Run one-hot on fulldf and then split


#age, km drive, rateper, mil_1, engine_new, powe_new, seats, price

fulldf_rest <- fulldf%>%
  select(age, Kilometers_Driven, rateperunit, mil_1, engine_new, Power_new, Seats, Price, status)

fulldf_few[ ,c(3,4,7,9,10)] <- map(fulldf_few[ ,c(3,4,7,9,10)], as.factor)%>%as.data.frame()

f <- dummyVars(" ~ V1+ V2 + Location + Year + Fuel_Type + Transmission + Owner_Type", data = fulldf)
fulldf_0 <- as.data.frame(predict(f, fulldf))

fulldf_ohe <- bind_cols(fulldf_0, fulldf_rest)%>%
  select(-V1,-V2)

colSums(is.na(fulldf_ohe))
glimpse(fulldf_ohe)
colnames(fulldf_ohe) <- str_replace_all(colnames(fulldf_ohe), "[^[:alnum:]]", "" )

#Work more on it
colnames(fulldf_ohe)[68] <- "V2BRV2"
colnames(fulldf_ohe)[233] <- "V2WRV2"

fulldf_ohe$KilometersDriven <- as.numeric(as.character(fulldf_ohe$KilometersDriven))


write_xlsx(fulldf_ohe, "fulldf_Onehotencoded.xlsx")

train <- fulldf_ohe%>%filter(status == "train")

randomForest(Price~ ., )
