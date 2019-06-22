
# TVC and AUC -------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)


TVC <- read_xlsx("C:/Users/Patrick Jane/Downloads/TVC.xlsx")
AUC <- read_xlsx("C:/Users/Patrick Jane/Downloads/AUC.xlsx")

t <- TVC%>%select(1:4)

a <- AUC%>%
  select(1:4,6,8)

glimpse(a)
a$`Machine In Use Time(HH)` <- as.numeric(a$`Machine In Use Time(HH)`)
a$`Lock Time(HH)` <- as.numeric(a$`Lock Time(HH)`)
a$`Idle Time(HH)` <- as.numeric(a$`Idle Time(HH)`)

a <- a%>%
  mutate(auctime = `Machine In Use Time(HH)`+`Lock Time(HH)`+`Idle Time(HH)`)

InAUC_butnotinTVC <- a%>%
  filter(!`Practitioner ID` %in% t$`Practitioner ID`)

InTVC_butnotinAUC <- t%>%
  filter(!`Practitioner ID` %in% a$`Practitioner ID`)

joined_data <- t%>%
  inner_join(a)

joined_data <- joined_data%>%
  mutate(diff = `Total Logged Time at Work Hrs` - auctime)%>%
  arrange(diff)

write_xlsx(list(InAUCNoTVC = InAUC_butnotinTVC, InTVCNoAUV = InTVC_butnotinAUC, FinalSheet = joined_data), "OutputSheet.xlsx")





