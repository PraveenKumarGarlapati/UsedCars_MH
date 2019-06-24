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


fulldf$V2[fulldf$V2 == "Flying"] = "Continental"
fulldf$V2[fulldf$V2 == "Land"] = "Fortuner"
fulldf$V2[fulldf$V2 == "MU"] = "MUX"
fulldf$V2[fulldf$V2 == "370Z"] = "Terrano"
fulldf$V2[fulldf$V2 == "Abarth"] = "Punto"

fulldf$V2[fulldf$V2 == "Motors"] = "800"
fulldf$V2[fulldf$V2 == "1.4Gsi"] = "800"

fulldf$V1[fulldf$V1 == "Hindustan"] = "Maruti"
fulldf$V1[fulldf$V1 == "OpelCorsa"] = "Maruti"



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

lm_pred <- lm_pred%>%mutate(Price = abs(Price))
write_xlsx(lm_pred, "lm_pred.xlsx")

#######
#Replacing all the Nonmatching V2 Values as well with relavant ones
Data_Test%>%
  filter(!Name %in% Data_Train$Name)

test_t%>%
  filter(!V2 %in% train_t$V2)

test_t$V2[test_t$V2 == "Flying"] = "Continental"
test_t$V2[test_t$V2 == "Land"] = "Fortuner"
test_t$V2[test_t$V2 == "MU"] = "MUX"
test_t$V2[test_t$V2 == "370Z"] = "Terrano"
test_t$V2[test_t$V2 == "Abarth"] = "Punto"

test_t$V2[test_t$V2 == "Motors"] = "800"
test_t$V2[test_t$V2 == "1.4Gsi"] = "800"

test_t$V1[test_t$V1 == "Hindustan"] = "Maruti"
test_t$V1[test_t$V1 == "OpelCorsa"] = "Maruti"



lm = lm(Price ~ ., train_t[ ,-c(5,6)])
lm_pred <- predict(lm, test_t)

lm_pred <- lm_pred%>%as.data.frame()
colnames(lm_pred) <- "Price"

#Took absolute values of predictions
write_xlsx(lm_pred, "lm_pred_w_V2.xlsx")

#DT
dt = ctree(Price ~ ., train_t[ ,-6])
dt_pred <- predict(dt, test_t)

dt_pred <- as.data.frame(dt_pred)
colnames(dt_pred) <- "Price"

write_xlsx(dt_pred, "dt_pred_.xlsx")

##Adding price of fuel and getting running cost per kilometre

fulldf$rateperunit[is.na(fulldf$rateperunit)] = 2
fulldf$rateperunit[which(fulldf$rateperunit==Inf)] = NA

#Removing Inf values in rateperunit col
fulldf%>%
  filter(is.na(rateperunit))%>%
  count(Fuel_Type)

fulldf%>%
  group_by(Fuel_Type)%>%
  summarise(mean(rateperunit,na.rm = T))

fulldf$rateperunit[is.na(fulldf$rateperunit)] = 4

####



train_t <- fulldf%>%
  filter(status == "train")%>%
  select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Fuel_Type, rateperunit, Price)
train_t%>%glimpse()

train_t$Owner_Type <- as.factor(train_t$Owner_Type)
train_t$Mileage_new[is.na(train_t$Mileage_new)] = mean(train_t$Mileage_new, na.rm = TRUE)
train_t$Fuel_Type <- as.factor(train_t$Fuel_Type)

test_t <- fulldf%>%
  filter(status == "test")%>%
  select(Kilometers_Driven, Owner_Type, V1, V2, Mileage_new, avgnewpriceV1V2, age, Fuel_Type, rateperunit, Price)
test_t%>%glimpse()

test_t$Owner_Type <- as.factor(test_t$Owner_Type)
test_t$Mileage_new[is.na(test_t$Mileage_new)] = mean(test_t$Mileage_new, na.rm = TRUE)
test_t$Fuel_Type <- as.factor(test_t$Fuel_Type)

# fulldf$rateperunit[which(fulldf$rateperunit==Inf)] = NA
# test_t$rateperunit[which(test_t$rateperunit==Inf)] = NA

#LM
lm = lm(Price ~ . , train_t[ ,-c(5,6)])
lm_pred <- predict(lmodel, test)

lm_pred <- lm_pred%>%as.data.frame()
colnames(lm_pred) <- "Price"

#Took absolute values of predictions
write_xlsx(lm_pred, "lm_pred_all.xlsx")


#DT
dt = ctree(Price ~ ., train_t[ ,-c(5,6)])
dt_pred <- predict(dt, test_t)

dt_pred <- as.data.frame(dt_pred)
colnames(dt_pred) <- "Price"

write_xlsx(dt_pred, "dt_pred_.xlsx")

#Not able to run this dtree

library(rpart)
library(rpart.plot)
dtfit <- rpart(Price~., data = train_t[ ,-c(6)], method = 'anova')

# test_t_m <- as.matrix(test_t)

dt_pred <- predict(dtfit, test_t)

dt_pred <- as.data.frame(dt_pred)
colnames(dt_pred) <- "Price"

write_xlsx(dt_pred, "dt_pred_rateperunit_wmil.xlsx")


##
train_t%>%glimpse()
test_t%>%glimpse()
test_t%>%
  filter(Kilometers_Driven %in% train_t$Kilometers_Driven)

#Taking units of Mileage

mileage_brkup = str_split_fixed(fulldf$Mileage, " ",2)
mileage_brkup <- as.data.frame(mileage_brkup)

colnames(mileage_brkup) <- c("M_brkup_1","M_brkup_2")
fulldf <- bind_cols(fulldf, mileage_brkup)

fulldf$M_brkup_11 <- as.numeric(fulldf$M_brkup_1)
rate <- read_clip_tbl()

fulldf <- fulldf%>%
  left_join(rate)

fulldf <- fulldf%>%
  mutate(rateperunit = Rate/M_brkup_11)


###############3



train <- fulldf%>%
  filter(status == "train")%>%
  select(V1,V2,
         Location,
         age,
         Kilometers_Driven,
         Fuel_Type,
         rateperunit,
         Transmission,
         Owner_Type,
         Mileage_new,
         engine_new,
         Power_new,
         Seats,
         Price)

test <- fulldf%>%
  filter(status == "test")%>%
  select(V1,V2,
         Location,
         age,
         Kilometers_Driven,
         Fuel_Type,
         rateperunit,
         Transmission,
         Owner_Type,
         Mileage_new,
         engine_new,
         Power_new,
         Seats,
         Price)

glimpse(train)
dim(test)

lmodel <- lm(Price ~ ., data = train)
lm_pred <- predict(lmodel, test)
lm_pred <- as.data.frame(lm_pred)
colnames(lm_pred) <- "Price"
bind_cols(test, lm_pred)%>%View()
lm_pred <- lm_pred%>%
  mutate(Price = abs(Price))
# lm_pred$Price[is.na(lm_pred$Price)] <- mean(lm_pred$Price, na.rm = T)

colSums(is.na(fulldf))
colSums(is.na(train))

fulldf%>%
  filter(is.na(engine_new1))%>%
  count(V1,V2)

fulldf%>%
  filter(is.na(Power_new1))%>%
  count(V1,V2)

fulldf%>%
  filter(is.na(Seats1))%>%
  count(V1,V2)


fulldf <- fulldf%>%
  group_by(V1,V2)%>%
  mutate(engine_new1 = ifelse(is.na(engine_new), mean(engine_new, na.rm = T), engine_new))

fulldf <- fulldf%>%
  group_by(V1,V2)%>%
  mutate(Power_new1 = ifelse(is.na(Power_new), mean(Power_new, na.rm = T), Power_new))

fulldf <- fulldf%>%
  group_by(V1,V2)%>%
  mutate(Seats1 = ifelse(is.na(Seats), mean(Seats, na.rm = T), Seats))

colSums(is.na(fulldf))

fulldf_bckup <- fulldf

fulldf$engine_new[is.na(fulldf$engine_new)] <- mean(fulldf$engine_new, na.rm = T)
fulldf$Power_new[is.na(fulldf$Power_new)] <- mean(fulldf$Power_new, na.rm = T)
fulldf$rateperunit[is.na(fulldf$rateperunit)] <- mean(fulldf$rateperunit, na.rm = T)
fulldf$Seats[is.na(fulldf$Seats)] <- mean(fulldf$Seats, na.rm = T)
fulldf$Mileage_new[is.na(fulldf$Mileage_new)] <- 30


glimpse(fulldf)
fulldf[ ,c(1:2,5:10,12,14,26)] <- map(fulldf[ ,c(1:2,5:10,12,14,26)], as.factor)

dt <- ctree(Price ~ ., train)
dt_pred <- predict(dt, test)
dt_pred <- as.data.frame(dt_pred)
colnames(dt_pred) <- "Price"
bind_cols(test, lm_pred)%>%View()

write_xlsx(dt_pred, "dt_all.xlsx")


#scale all the numeric columns and run the model again. 


