# Load Libraries

library(tidyverse)
library(readxl)
library(dbplyr)
library(data.table)

# Load Data
df.master <- read_excel("data/Customer Master 3_2019to2_2020 v5.xlsx", sheet = "Sheet1",col_names = TRUE, col_types = NULL, na = "") %>%
  mutate(Customer_Number = as.integer(Customer_Number))


# General cleanup and tie-ins ####

# Calculate Annualize and Totals of BC/PM
df.master$AnnualBCVolume<-52*df.master$BCVolume/df.master$NbrActiveWeeks
df.master$AnnualBIBVolume<-52*df.master$BIBVolume/df.master$NbrActiveWeeks
df.master$AnnualTotalVolume<-df.master$AnnualBIBVolume+df.master$AnnualBCVolume

df.master$AnnualBCRev<-52*df.master$BCDeadNetRevenue/df.master$NbrActiveWeeks
df.master$AnnualBIBRev<-52*df.master$BIBDeadNetRevenue/df.master$NbrActiveWeeks
df.master$AnnualTotalRev<-df.master$AnnualBIBRev+df.master$AnnualBCRev

# Clean Data
df.master <- df.master %>%
  filter(PlannedFreq>0,
         AnnualTotalVolume > 0,
         `AnnualTotalRev` > 0,
         `TotalNbrStops` > 0,
         PreferredOrderMethod != "SUPPRESSED",
         BusinessType != "Staff Customer")

cust_info <- fread("data/cust_info-EB-C73154.csv") %>%
  mutate(Customer_Number = as.integer(`Customer Number`)) %>%
  select(Customer_Number, `Preferred Ordering Method`, `Primary Group`, `Secondary Group`)

df.master <- df.master %>%
  select(-c(PreferredOrderMethod)) %>%
  left_join(cust_info) %>%
  filter(`Preferred Ordering Method` != "")

# indicate if ntnl cust

ntl.act <- read_excel("data/National Account List.xlsx") %>%
  select(PG, SG) %>%
  mutate(national = "national")

df.master <- df.master %>%
  left_join(ntl.act, by = c("Primary Group" = "PG")) %>%
  left_join(ntl.act, by = c("Secondary Group" = "SG"))

df.master$national.x[is.na(df.master$national.x)] <- as.character(df.master$national.y[is.na(df.master$national.x)])

df.master <- df.master %>%
  select(-c(national.y, SG, PG)) %>%
  rename(national = national.x) %>%
  replace_na(list(national = "no"))

table(df.master$`Preferred Ordering Method`)

rm(cust_info, ntl.act)

# Distribution Calculations

df.master$TotalRevPerStop<-df.master$AnnualTotalRev/df.master$PlannedFreq
df.master$TotalRevPerStop<-as.numeric(df.master$TotalRevPerStop)
df.master$BIBRevPerStop<-df.master$AnnualBIBRev/df.master$PlannedFreq
df.master$BIBRevPerStop<-as.numeric(df.master$BIBRevPerStop)
df.master$BCRevPerStop<-df.master$AnnualBCRev/df.master$PlannedFreq
df.master$BCRevPerStop<-as.numeric(df.master$BCRevPerStop)
df.master$TotalRevPerStop<-ifelse(df.master$BIBRevPerStop>=0 & df.master$BCRevPerStop>=0,df.master$BIBRevPerStop+df.master$BCRevPerStop,
                                  ifelse (df.master$BIBRevPerStop<0 & df.master$BCRevPerStop>=0,df.master$BCRevPerStop,
                                          ifelse(df.master$BIBRevPerStop>=0 & df.master$BCRevPerStop<0,df.master$BIBRevPerStop,df.master$BIBRevPerStop+df.master$BCRevPerStop)))
df.master$TotalRevPerStop<-as.numeric(df.master$TotalRevPerStop)
df.master$TotalRevPerStop26<-ifelse(df.master$BIBRevPerStop>=0 & df.master$BCRevPerStop>=0,(df.master$AnnualBIBRev+df.master$AnnualBCRev)/26,
                                                               ifelse (df.master$BIBRevPerStop<0 & df.master$BCRevPerStop>=0,(df.master$AnnualBCRev)/26,
                                                                       ifelse(df.master$BIBRevPerStop>=0 & df.master$BCRevPerStop<0,(df.master$AnnualBIBRev)/26,(df.master$AnnualBIBRev+df.master$AnnualBCRev)/26)))

df.master$BIBCasesPerStop<-df.master$AnnualBIBVolume/df.master$PlannedFreq
df.master$BIBCasesPerStop<-as.numeric(df.master$BIBCasesPerStop)
df.master$BCCasesPerStop<-df.master$AnnualBCVolume/df.master$PlannedFreq
df.master$BCCasesPerStop<-as.numeric(df.master$BCCasesPerStop)
df.master$TotalCasesPerStop<-ifelse(df.master$BIBCasesPerStop>=0 & df.master$BCCasesPerStop>=0,df.master$BIBCasesPerStop+df.master$BCCasesPerStop,
                                    ifelse (df.master$BIBCasesPerStop<0 & df.master$BCCasesPerStop>=0,df.master$BCCasesPerStop,
                                            ifelse(df.master$BIBCasesPerStop>=0 & df.master$BCCasesPerStop<0,df.master$BIBCasesPerStop,"CHECK")))

df.master$TotalCasesPerStop<-as.numeric(df.master$TotalCasesPerStop)
df.master$ProductFlag<-ifelse(df.master$AnnualBCVolume>0 & df.master$AnnualBIBVolume>0, "BC and PM",
                              ifelse(df.master$AnnualBIBVolume>0 & df.master$AnnualBCVolume<=0,"PM Only",
                                     ifelse(df.master$AnnualBCVolume>0 & df.master$AnnualBIBVolume<=0,"BC Only","ERROR")))

df.master$TotalCasesPerStop26<-ifelse(df.master$BIBCasesPerStop>=0 & df.master$BCCasesPerStop>=0,(df.master$AnnualBIBVolume+df.master$AnnualBCVolume)/26,
                                    ifelse (df.master$BIBCasesPerStop<0 & df.master$BCCasesPerStop>=0,(df.master$AnnualBCVolume)/26,
                                            ifelse(df.master$BIBCasesPerStop>=0 & df.master$BCCasesPerStop<0,df.master$AnnualBIBVolume/26,"CHECK")))

df.master$TotalCasesPerStop26<-as.numeric(df.master$TotalCasesPerStop26)

df.master$IncreaseRev<-(df.master$AnnualTotalVolume*df.master$IncreasePricing)
df.master$TotalRevPerStopIP<-df.master$TotalRevPerStop+(df.master$IncreaseRev/df.master$PlannedFreq)

df.master$TotalRevPerStopIP26<-df.master$TotalRevPerStop+(df.master$IncreaseRev/26)



df.master$CaseMax<-10
df.master$CaseMaxDD<-1000
df.master$BCMaxRev<-500
df.master$BIBMaxRev<-350


# Distribution ####

df.master$NewDeliveryMethod<-with(df.master,ifelse(df.master$national=="national",df.master$DeliveryType,
                                                   ifelse(df.master$Channel=="Club Store" & df.master$TotalCasesPerStop>df.master$CaseMaxDD|df.master$Channel=="Wholesale" & df.master$TotalCasesPerStop>df.master$CaseMaxDD|df.master$Channel=="RE-SELLER" & df.master$TotalCasesPerStop>df.master$CaseMaxDD|df.master$Channel=="FULL LINE OPERATOR" & df.master$TotalCasesPerStop>df.master$CaseMaxDD,"Direct Delivery",
                                                          ifelse(df.master$TotalRevPerStop>=df.master$BCMaxRev|df.master$BCRevPerStop>=df.master$BCMaxRev|df.master$BIBRevPerStop>=df.master$BIBMaxRev,"DSD",
                                                                 ifelse(df.master$ProductFlag=="PM Only" & df.master$TotalRevPerStop26>=df.master$BIBMaxRev|df.master$TotalRevPerStop26>=df.master$BCMaxRev,"DSD Reduce Frequency",
                                                                        ifelse(df.master$ProductFlag=="PM Only" & df.master$TotalRevPerStopIP>=df.master$BIBMaxRev|df.master$TotalRevPerStopIP>=df.master$BCMaxRev,"DSD Increase Pricing",
                                                                               ifelse(df.master$ProductFlag=="PM Only" & df.master$TotalRevPerStopIP26>=df.master$BIBMaxRev|df.master$TotalRevPerStopIP26>=df.master$BCMaxRev,"DSD RF & IP",
                                                                                      ifelse(df.master$PlannedFreq<=26 & df.master$TotalCasesPerStop<df.master$CaseMax & df.master$IncreasePricing==0,"Parcel",
                                                                                             ifelse(df.master$TotalCasesPerStop26<df.master$CaseMax & df.master$IncreasePricing<=0, "Parcel RF",
                                                                                                    ifelse(df.master$TotalCasesPerStop<df.master$CaseMax & df.master$IncreasePricing>0 , "Parcel IP",
                                                                                                           ifelse(df.master$TotalCasesPerStop26<df.master$CaseMax & df.master$IncreasePricing>0 & df.master$PlannedFreq>=26, "Parcel RF & IP",
                                                                                                                  ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$PlannedFreq<=26 & df.master$ProductFlag!="PM Only" & df.master$IncreasePricing==0,"Local ARTM Distribution Partner",
                                                                                                                         ifelse(df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag!="PM Only"& df.master$IncreasePricing==0,"Local ARTM Distribution Partner RF",
                                                                                                                                ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$ProductFlag!="PM Only" & df.master$IncreasePricing>0 ,"Local ARTM Distribution Partner IP",
                                                                                                                                       ifelse(df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag!="PM Only","Local ARTM Distribution Partner RF and IP",
                                                                                                                                              ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$PlannedFreq<=26 & df.master$ProductFlag=="PM Only" & df.master$IncreasePricing==0,"Foodservice ARTM Distributor",
                                                                                                                                                     ifelse(df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag=="PM Only"& df.master$IncreasePricing==0,"Foodservice ARTM Distributor RF",
                                                                                                                                                            ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$ProductFlag=="PM Only" & df.master$IncreasePricing>0 ,"Foodservice ARTM Distributor IP",
                                                                                                                                                                   ifelse(df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag=="PM Only","Foodservice ARTM DistributorRF and IP","CHECK")))))))))))))))))))

# LocalEatingandDrinking

df.master$NewDeliveryMethod<-with(df.master,ifelse(df.master$KeyAccount!="SPECIAL EMPHASIS",df.master$NewDeliveryMethod,
                                                   ifelse(df.master$national=="national",df.master$DeliveryType,
                                                          ifelse(df.master$Channel=="Club Store"& df.master$TotalCasesPerStop>df.master$CaseMaxDD|df.master$Channel=="RE-SELLER" & df.master$TotalCasesPerStop>df.master$CaseMaxDD|df.master$Channel=="FULL LINE OPERATOR" & df.master$TotalCasesPerStop>df.master$CaseMaxDD,"Direct Delivery",
                                                                                                   ifelse(df.master$TotalRevPerStop>=df.master$BCMaxRev|df.master$BCRevPerStop>=df.master$BCMaxRev|df.master$BIBRevPerStop>=df.master$BIBMaxRev,"DSD",
                                                                                                          ifelse(df.master$ProductFlag=="PM Only" & df.master$TotalRevPerStop26*2>=df.master$BIBMaxRev|df.master$TotalRevPerStop26*2>=df.master$BCMaxRev,"DSD Reduce Frequency",
                                                                                                                 ifelse(df.master$ProductFlag=="PM Only" & df.master$TotalRevPerStopIP>=df.master$BIBMaxRev|df.master$TotalRevPerStopIP>=df.master$BCMaxRev,"DSD Increase Pricing",
                                                                                                                        ifelse(df.master$ProductFlag=="PM Only" & 2*df.master$TotalRevPerStopIP26>=df.master$BIBMaxRev|2*df.master$TotalRevPerStopIP26>=df.master$BCMaxRev,"DSD RF & IP",
                                                                                                                               ifelse(df.master$PlannedFreq<=13 & df.master$TotalCasesPerStop<df.master$CaseMax & df.master$IncreasePricing==0,"Parcel",
                                                                                                                                      ifelse(2*df.master$TotalCasesPerStop26<df.master$CaseMax & df.master$IncreasePricing<=0, "Parcel RF",
                                                                                                                                             ifelse(df.master$TotalCasesPerStop<df.master$CaseMax & df.master$IncreasePricing>0 , "Parcel IP",
                                                                                                                                                    ifelse(2*df.master$TotalCasesPerStop26<df.master$CaseMax & df.master$IncreasePricing>0 & df.master$PlannedFreq>=13, "Parcel RF & IP",
                                                                                                                                                           ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$PlannedFreq<=13 & df.master$ProductFlag!="PM Only" & df.master$IncreasePricing==0,"Local ARTM Distribution Partner",
                                                                                                                                                                  ifelse(2*df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag!="PM Only"& df.master$IncreasePricing==0,"Local ARTM Distribution Partner RF",
                                                                                                                                                                         ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$ProductFlag!="PM Only" & df.master$IncreasePricing>0 ,"Local ARTM Distribution Partner IP",
                                                                                                                                                                                ifelse(2*df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag!="PM Only","Local ARTM Distribution Partner RF and IP",
                                                                                                                                                                                       ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$PlannedFreq<=13 & df.master$ProductFlag=="PM Only" & df.master$IncreasePricing==0,"Foodservice ARTM Distributor",
                                                                                                                                                                                              ifelse(2*df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag=="PM Only"& df.master$IncreasePricing==0,"Foodservice ARTM Distributor RF",
                                                                                                                                                                                                     ifelse(df.master$TotalCasesPerStop>=df.master$CaseMax & df.master$ProductFlag=="PM Only" & df.master$IncreasePricing>0 ,"Foodservice ARTM Distributor IP",
                                                                                                                                                                                                            ifelse(2*df.master$TotalCasesPerStop26>=df.master$CaseMax & df.master$ProductFlag=="PM Only","Foodservice ARTM DistributorRF and IP","CHECK"))))))))))))))))))))
df.master$NewDeliveryMethod<-with(df.master,ifelse(df.master$Channel!="Club Store",df.master$NewDeliveryMethod,
                                                   ifelse(df.master$TotalCasesPerStop>df.master$CaseMaxDD,"Direct Delivery","DSD")))

df.master$NewDeliveryMethod[df.master$NewDeliveryMethod=="SIDELOAD - OFS"|df.master$NewDeliveryMethod=="BULK"]<-"DSD"

write.csv(df.master,"NewDelMode3.csv")


# Order method ####

df.master

# 30105

# see types of ordering method

table(df.master$`Preferred Ordering Method`)

# see types of new del

table(df.master$NewDeliveryMethod)

df.master <- df.master %>%
  mutate(`Preferred Ordering Method` = recode(`Preferred Ordering Method`,
                                              "CALL" = "CCC",
                                              "ECOM" = "MYCOKE",
                                              "EDI" = "EDI",
                                              "EDID" = "EDID",
                                              "EML" = "CCC",
                                              "FAX" = "CCC",
                                              "HOUS" = "CCC",
                                              "INCL" = "CCC",
                                              "SREP" = "F2F"))

table(df.master$`Preferred Ordering Method`)

# remove EDI/D rows since those won't change, tie back in later.

tie.in.1 <- df.master %>%
  filter(`Preferred Ordering Method` == "EDI" |
           `Preferred Ordering Method` == "EDID")

df <- df.master %>%
  anti_join(tie.in.1, by = NULL)

tie.in.1 <- tie.in.1 %>%
  mutate(`NEW Preferred Ordering Method` = `Preferred Ordering Method`)

# tie in 1 + df

nrow(tie.in.1) + nrow(df) == nrow(df.master)

# Keep F2F national key accounts as such

tie.in.2 <- df %>%
  filter(`Preferred Ordering Method` == "F2F" & national == "national")

df <- df %>%
  anti_join(tie.in.2, by = NULL)

tie.in.2 <- tie.in.2 %>%
  mutate(`NEW Preferred Ordering Method` = `Preferred Ordering Method`)

# tie in 1 + tie in 2 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(df) == nrow(df.master)

# Keep CCC national key accounts as such

tie.in.3 <- df %>%
  filter(national == "national") %>%
  filter(`Preferred Ordering Method` == "CCC" )

df <- df %>%
  anti_join(tie.in.3, by = NULL)

tie.in.3 <- tie.in.3 %>%
  mutate(`NEW Preferred Ordering Method` = `Preferred Ordering Method`)

# tie in 1 + tie in 2 + tie in 3 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(df) == nrow(df.master)

# red truck and retail to F2F

tie.in.4 <- df %>%
  filter(NewDeliveryMethod == "DirectDelivery" |
           NewDeliveryMethod == "DSD" |
           NewDeliveryMethod == "DSD Increase Pricing" |
           NewDeliveryMethod == "DSD Reduce Frequency" |
           NewDeliveryMethod == "DSD RF & IP",
         FSOP_RetailFlag == "Retail"
  )

df <- df %>%
  anti_join(tie.in.4, by = NULL)

tie.in.4 <- tie.in.4 %>%
  mutate(`NEW Preferred Ordering Method` = "F2F")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(tie.in.4) + nrow(df) == nrow(df.master)

# red truck and FSOP to MyCoke

tie.in.5 <- df %>%
  filter(NewDeliveryMethod == "DirectDelivery" |
           NewDeliveryMethod == "DSD" |
           NewDeliveryMethod == "DSD Increase Pricing" |
           NewDeliveryMethod == "DSD Reduce Frequency" |
           NewDeliveryMethod == "DSD RF & IP",
         FSOP_RetailFlag == "FSOP"
  )

df <- df %>%
  anti_join(tie.in.5, by = NULL)

tie.in.5 <- tie.in.5 %>%
  mutate(`NEW Preferred Ordering Method` = "MYCOKE")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + tie in 5 df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(tie.in.4) + nrow(tie.in.5) + nrow(df) == nrow(df.master)

# Parcel to MyCoke

tie.in.6 <- df %>%
  filter(NewDeliveryMethod == "Parcel" |
           NewDeliveryMethod == "Parcel IP" |
           NewDeliveryMethod == "Parcel RF")

df <- df %>%
  anti_join(tie.in.6, by = NULL)

tie.in.6 <- tie.in.6 %>%
  mutate(`NEW Preferred Ordering Method` = "MYCOKE")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + tie in 5 + tie in 6 df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(tie.in.4) + nrow(tie.in.5) + nrow(tie.in.6) + nrow(df) == nrow(df.master)

# reseller/distributor to N/A

table(df.master$Channel)

tie.in.7 <- df %>%
  filter(Channel == "FULL LINE OPERATOR" |
           Channel == "Wholesale" |
           Channel == "RE-SELLER")

df <- df %>%
  anti_join(tie.in.7, by = NULL)

tie.in.7 <- tie.in.7 %>%
  mutate(`NEW Preferred Ordering Method` = "reseller/distributor")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + tie in 5 df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(tie.in.4) + nrow(tie.in.5) + nrow(tie.in.6) + nrow(tie.in.7) + nrow(df) == nrow(df.master)

# any remainder?

remainder <- df %>%
  select(Name, SecondaryGroup, Channel, TotalCasesPerStop, DeliveryType, KeyAccount, SubChannel, FSOP_RetailFlag, national, ProductFlag, NewDeliveryMethod)

# ARTM to MyCoke

tie.in.8 <- df %>%
  filter(NewDeliveryMethod == "Local ARTM Distribution Partner" |
           NewDeliveryMethod == "Foodservice ARTM Distributor" |
           NewDeliveryMethod == "Foodservice ARTM Distributor RF" |
           NewDeliveryMethod == "Local ARTM Distribution Partner IP" |
           NewDeliveryMethod == "Local ARTM Distribution Partner RF")

df <- df %>%
  anti_join(tie.in.8, by = NULL)

tie.in.8 <- tie.in.8 %>%
  mutate(`NEW Preferred Ordering Method` = "MYCOKE")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + tie in 5,6,7,8 df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(tie.in.4) + nrow(tie.in.5) + nrow(tie.in.6) + nrow(tie.in.7) + nrow(df) + nrow(tie.in.8) == nrow(df.master)

remainder <- df %>%
  mutate(`NEW Preferred Ordering Method` = "MYCOKE")



# bind together

df <- rbind(tie.in.1, tie.in.2, tie.in.3, tie.in.4, tie.in.5, tie.in.6, tie.in.7, tie.in.8, remainder)

rm(tie.in.1, tie.in.2, tie.in.3, tie.in.4, tie.in.5, tie.in.6, tie.in.7, remainder, tie.in.8)

from.to.order <- df %>%
  select(`Preferred Ordering Method`, `NEW Preferred Ordering Method`) %>%
  group_by(`Preferred Ordering Method`, `NEW Preferred Ordering Method`) %>%
  count() %>%
  arrange(-n) %>%
  rename(count = n)


# Equipment ####

table(df.master$NewDeliveryMethod)

# red truck no charge

tie.in.1 <- df %>%
  filter(NewDeliveryMethod == "DirectDelivery" |
           NewDeliveryMethod == "DSD" |
           NewDeliveryMethod == "DSD Increase Pricing" |
           NewDeliveryMethod == "DSD Reduce Frequency" |
           NewDeliveryMethod == "DSD RF & IP")

df <- df %>%
  anti_join(tie.in.1, by = NULL)

tie.in.1 <- tie.in.1 %>%
  mutate(`Equipment Charge` = "No Charge")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + df

nrow(tie.in.1) + nrow(df) == nrow(df.master)

# distributor >300 G no charge

tie.in.2 <- df %>%
  filter(NewDeliveryMethod == "Local ARTM Distribution Partner" |
           NewDeliveryMethod == "Foodservice ARTM Distributor" |
           NewDeliveryMethod == "Foodservice ARTM Distributor RF" |
           NewDeliveryMethod == "Local ARTM Distribution Partner IP" |
           NewDeliveryMethod == "Local ARTM Distribution Partner RF",
         BIBVolume > 300)

df <- df %>%
  anti_join(tie.in.2, by = NULL)

tie.in.2 <- tie.in.2 %>%
  mutate(`Equipment Charge` = "No Charge")

# tie in 1 + tie in 2 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(df) == nrow(df.master)

# distributor <300 G charge

tie.in.3 <- df %>%
  filter(NewDeliveryMethod == "Local ARTM Distribution Partner" |
           NewDeliveryMethod == "Foodservice ARTM Distributor" |
           NewDeliveryMethod == "Foodservice ARTM Distributor RF" |
           NewDeliveryMethod == "Local ARTM Distribution Partner IP" |
           NewDeliveryMethod == "Local ARTM Distribution Partner RF",
         BIBVolume < 300)

df <- df %>%
  anti_join(tie.in.3, by = NULL)

tie.in.3 <- tie.in.3 %>%
  mutate(`Equipment Charge` = "Rental/Service Charge")

# tie in 1 + tie in 2 + tie in 3 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3) + nrow(df) == nrow(df.master)

# parcel low cost cooler

tie.in.4 <- df %>%
  filter(NewDeliveryMethod == "Parcel" |
           NewDeliveryMethod == "Parcel IP" |
           NewDeliveryMethod == "Parcel RF")

df <- df %>%
  anti_join(tie.in.4, by = NULL)

tie.in.4 <- tie.in.4 %>%
  mutate(`Equipment Charge` = "Low cost cooler only ($185/cooler.  One time placement.  No service)")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3)  + nrow(tie.in.4) + nrow(df) == nrow(df.master)

# local artm >104

tie.in.5 <- df %>%
  filter(NewDeliveryMethod == "Local ARTM Distribution Partner" |
           NewDeliveryMethod == "Local ARTM Distribution Partner IP" |
           NewDeliveryMethod == "Local ARTM Distribution Partner RF",
         AnnualBCVolume > 104)

df <- df %>%
  anti_join(tie.in.5, by = NULL)

tie.in.5 <- tie.in.5 %>%
  mutate(`Equipment Charge` = "No charge")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + tie in 5 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3)  + nrow(tie.in.4) + nrow(tie.in.5) + nrow(df) == nrow(df.master)

# local artm <104

tie.in.6 <- df %>%
  filter(NewDeliveryMethod == "Local ARTM Distribution Partner" |
           NewDeliveryMethod == "Local ARTM Distribution Partner IP" |
           NewDeliveryMethod == "Local ARTM Distribution Partner RF",
         AnnualBCVolume < 104)

df <- df %>%
  anti_join(tie.in.6, by = NULL)

tie.in.6 <- tie.in.6 %>%
  mutate(`Equipment Charge` = "Rental/Service Charge")

# tie in 1 + tie in 2 + tie in 3 + tie in 4 + tie in 5 + tie in 6 + df

nrow(tie.in.1) + nrow(tie.in.2) + nrow(tie.in.3)  + nrow(tie.in.4) + nrow(tie.in.5) + nrow(tie.in.6) + nrow(df) == nrow(df.master)

# any remainder?

remainder <- df %>%
  select(Name, SecondaryGroup, Channel, DeliveryType, KeyAccount, SubChannel, FSOP_RetailFlag, national, ProductFlag, NewDeliveryMethod)

remainder <- df %>%
  mutate(`Equipment Charge` = "No Charge")

# bind together

df <- rbind(tie.in.1, tie.in.2, tie.in.3, tie.in.4, tie.in.5, tie.in.6, remainder)

rm(tie.in.1, tie.in.2, tie.in.3, tie.in.4, tie.in.5, tie.in.6, remainder)

write.csv(df, "outputs/new_categories.csv")
write.csv(from.to.order, "outputs/order_shift.csv")

table(df$`Equipment Charge`)


# Secondary Volume flag ####

sec.vol <- read_excel("data/sec_vol.xlsx")

sec.vol <- sec.vol %>%
  filter(`Secondary Volume` > 0) %>%
  select(-c(Customer, Volume)) %>%
  rename(Customer_Number = `Customer host code`) %>%
  mutate(`Secondary Volume` = "YES",
         Customer_Number = as.numeric(Customer_Number))

df <- df %>%
  left_join(sec.vol, by = "Customer_Number") %>%
  replace_na(list(`Secondary Volume` = "NO"))

write.csv(df, "outputs/new_categories.csv")