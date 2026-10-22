rm(list=ls())
library(readr)
library(tidyverse)
library(readxl)

#part 1: read and wrangle UPP data and abridge it by 5 age groups for analysis. 

# set wd

LE65bySex <- read_delim("route/LE65bySextxt.txt", 
                        delim = "\t", escape_double = FALSE,
                        trim_ws = TRUE)

UNWPPDeathsMales <- LE65bySex  %>%  
  filter(Sex=="Male")

# - Life tables Males

#840 = US, 32 = Arg, 76=Bra, 124=Can, 152=Chi, 170=Col, 188=CRI, 484=Mex, 604=Per

UNWPPDeathsMalesLong <- UNWPPDeathsMales %>% 
  select(c(1,2,7:19)) %>% 
  filter(Year %in% c(2000,2005,2010,2015,2019))

             

UNWPPDeathsFemales <- LE65bySex  %>%  
  filter(Sex=="Female")

# - Life tables Females

#840 = US, 32 = Arg, 76=Bra, 124=Can, 152=Chi, 170=Col, 188=CRI, 484=Mex, 604=Per

UNWPPDeathsFemalesLong <- UNWPPDeathsFemales %>% 
  select(c(1,2,7:19)) %>% 
  filter(Year %in% c(2000,2005,2010,2015,2019))




# Insert Pop into the deaths

DeathData <- rbind(UNWPPDeathsMalesLong,UNWPPDeathsFemalesLong)

# Open population data


# Read Pop File and set directory


PopFile <- read_excel("Excel Pop Americas.xlsx")
PopFile [is.na(PopFile )] <- 0

AllPop <- PopFile %>% 
  filter( Year %in% c(2000,2005,2010,2015,2019)) %>% 
  select(c(3,4,8,19:29))


AllPop <-AllPop %>%
  group_by(Country,Year,Sex) %>% 
  summarize(Pop50=sum(Pop50)*1000,
            Pop55=sum(Pop55)*1000,
            Pop60=sum(Pop60)*1000,
            Pop65=sum(Pop65)*1000,
            Pop70=sum(Pop70)*1000,
            Pop75=sum(Pop75)*1000,
            Pop80=sum(Pop80)*1000,
            Pop85=sum(Pop85)*1000,
            Pop90=sum(Pop90)*1000,
            Pop95=sum(Pop95+Pop100)*1000) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Pop")

AllPop$Pop <- round(AllPop$Pop,0)

AllPop$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

# and now COD info
# set directory where the WHO data is 
# I will use the WHO data by cause for proportion, but deaths from WPP. 

Morticd10_part1 <-  read_csv("Morticd10_part1", col_types = cols(Cause = col_character()))

Morticd10_part2 <-  read_csv("Morticd10_part2", col_types = cols(Cause = col_character()))

Morticd10_part3 <-  read_csv("Morticd10_part3", col_types = cols(Cause = col_character()))

Morticd10_part4 <-  read_csv("Morticd10_part4", col_types = cols(Cause = col_character()))

Morticd10_part5 <-  read_csv("Morticd10_part5", col_types = cols(Cause = col_character()))


library(readxl)



colnames(Morticd10_part1)
colnames(Morticd10_part2)
colnames(Morticd10_part3)
colnames(Morticd10_part4)
colnames(Morticd10_part5)

class(Morticd10_part1$Cause)
class(Morticd10_part2$Cause)
class(Morticd10_part3$Cause)
class(Morticd10_part4$Cause)
class(Morticd10_part5$Cause)
# 
table(is.na(Morticd10_part1$Cause))
table(is.na(Morticd10_part2$Cause))
table(is.na(Morticd10_part3$Cause))
table(is.na(Morticd10_part4$Cause))
table(is.na(Morticd10_part5$Cause))
# 

numeric_cols <- sapply(Morticd10_part1, is.numeric)

# Replace NA values with 0 in numeric columns
Morticd10_part1[numeric_cols][is.na(Morticd10_part1[numeric_cols])] <- 0

numeric_cols <- sapply(Morticd10_part2, is.numeric)

# Replace NA values with 0 in numeric columns
Morticd10_part2[numeric_cols][is.na(Morticd10_part2[numeric_cols])] <- 0

numeric_cols <- sapply(Morticd10_part3, is.numeric)

# Replace NA values with 0 in numeric columns
Morticd10_part3[numeric_cols][is.na(Morticd10_part3[numeric_cols])] <- 0

numeric_cols <- sapply(Morticd10_part4, is.numeric)

# Replace NA values with 0 in numeric columns
Morticd10_part4[numeric_cols][is.na(Morticd10_part4[numeric_cols])] <- 0

numeric_cols <- sapply(Morticd10_part5, is.numeric)

# Replace NA values with 0 in numeric columns
Morticd10_part5[numeric_cols][is.na(Morticd10_part5[numeric_cols])] <- 0




AllMort <- rbind(Morticd10_part1,Morticd10_part2,Morticd10_part3,Morticd10_part4,Morticd10_part5)

# country codes 2090 Canada, 2310 Mexico, 2120 Chile, 2070 Brazil, 2020 Argentina,
# 2140 Costa Rica 2130 Colombia, 2450,United States of America , 2370 Peru

# 

str(AllMort)
AllMort <- AllMort %>% 
  filter(Country=="2450"|Country=="2310"|Country=="2020"|
           Country=="2120"|Country=="2090"|Country=="2070"|Country=="2130"|
           Country=="2140"|Country=="2370", Year %in% c(2000,2005,2010,2015,2019)) %>% 
  dplyr::select(c(1,4,5,6,7,8,25:34)) 


AllMort[is.na(AllMort)] <- 0

AllMort$Country[AllMort$Country=="2450"] <- "United States of America"
AllMort$Country[AllMort$Country=="2020"] <- "Argentina"
AllMort$Country[AllMort$Country=="2070"] <- "Brazil"
AllMort$Country[AllMort$Country=="2090"] <- "Canada"
AllMort$Country[AllMort$Country=="2120"] <- "Chile"
AllMort$Country[AllMort$Country=="2130"] <- "Colombia"
AllMort$Country[AllMort$Country=="2140"] <- "Costa Rica"
AllMort$Country[AllMort$Country=="2310"] <- "Mexico"
AllMort$Country[AllMort$Country=="2370"] <- "Peru"



table(AllMort$Deaths23)


# Deaths16	Deaths at age 50-54 years
# Deaths17	Deaths at age 55-59 years
# Deaths18	Deaths at age 60-64 years
# Deaths19	Deaths at age 65-69 years
# Deaths20	Deaths at age 70-74 years
# Deaths21	Deaths at age 75-79 years
# Deaths22	Deaths at age 80-84 years
# Deaths23	Deaths at age 85-89 years
# Deaths24	Deaths at age 90-94 years
# Deaths25	Deaths at age 95 years and above .  Same logic for Pop.

# Causes of Death  C001 (all causes). Only CVD (CVD subclassifications (ischemic heart disease, stroke, and Rest heart diseases, code 1064) )
# list 104 (ICD 10)

table(AllMort$List)

# Filter rows where the Name starts with a given letter and assign a certain value in a different column

# CVD is I


CVDMort <- AllMort %>%
  mutate(CVD = ifelse(substr(Cause, 1, 1) == "I", "Yes", "No")) %>% 
  filter(CVD== "Yes" & Year %in% c(2000,2005,2010,2015,2019) & Sex != 9) %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")  



CVDMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

# Cancer is C

CancerMort <- AllMort %>%
  mutate(Cancer = ifelse(substr(Cause, 1, 1) == "C", "Yes", "No")) %>% 
  filter(Cancer== "Yes" & Year %in% c(2000,2005,2010,2015,2019)  & Sex != 9) %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")


CancerMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

# Metabolic is E

MetabolicMort <- AllMort %>%
  mutate(Metabolic = ifelse(substr(Cause, 1, 1) == "E", "Yes", "No")) %>% 
  filter(Metabolic== "Yes" & Year %in% c(2000,2005,2010,2015,2019) & Sex != 9) %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")


MetabolicMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

# Respiratory is J


RespMort <- AllMort %>%
  mutate(Resp = ifelse(substr(Cause, 1, 1) == "J", "Yes", "No")) %>% 
  filter(Resp== "Yes" & Year %in% c(2000,2005,2010,2015,2019) & Sex != 9) %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")


RespMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

# Nervous code G 

NervMort <- AllMort %>%
  mutate(Nerv = ifelse(substr(Cause, 1, 1) == "G", "Yes", "No")) %>% 
  filter(Nerv== "Yes" & Year %in% c(2000,2005,2010,2015,2019) & Sex != 9) %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")


NervMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

# External Causes from V01-Y89


ExternalMort <- AllMort %>%
  mutate(External = ifelse(substr(Cause, 1, 1) == c("V","W","X","Y"), "Yes", "No")) %>% 
  filter(External== "Yes" & Year %in% c(2000,2005,2010,2015,2019) & Sex != 9) %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")


ExternalMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))


NoCVDMort <- AllMort %>%
  mutate(CVD = ifelse(substr(Cause, 1, 1) == "I", "Yes", "No")) %>% 
  filter(CVD== "No" & Sex !="9"  &  Year %in% c(2000,2005,2010,2015,2019) & Cause !="AAA") %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")

# 

NoCVDMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))


# Rest 

RestofMort <- AllMort %>%
  filter(Sex !="9"  &  Year %in% c(2000,2005,2010,2015,2019) & Cause !="AAA") %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "All")

RestofMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))



# Ill Defined R

IllMort <- AllMort %>%
  mutate(Ill = ifelse(substr(Cause, 1, 1) == "R", "Yes", "No")) %>% 
  filter(Ill== "Yes" & Sex !="9"  &  Year %in% c(2000,2005,2010,2015,2019) & Cause !="AAA") %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")

IllMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))



SumMort <- AllMort %>%
  filter(Sex !="9"  &  Year %in% c(2000,2005,2010,2015,2019) & Cause !="AAA") %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(Deaths50=sum(Deaths16),
            Deaths55=sum(Deaths17),
            Deaths60=sum(Deaths18),
            Deaths65=sum(Deaths19),
            Deaths70=sum(Deaths20),
            Deaths75=sum(Deaths21),
            Deaths80=sum(Deaths22),
            Deaths85=sum(Deaths23),
            Deaths90=sum(Deaths24),
            Deaths95=sum(Deaths25)) %>% 
  pivot_longer(cols = 4:13, names_to = "Age_Groups", values_to = "Deaths")

IllMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

IllMort$AllDeaths <- SumMort$Deaths
IllMort$ShareIll <- IllMort$Deaths/IllMort$AllDeaths

RestofMort$CVD <- round(CVDMort$Deaths + (CVDMort$Deaths * IllMort$ShareIll),0) 
RestofMort$Cancer <- round(CancerMort$Deaths + (CancerMort$Deaths * IllMort$ShareIll),0)
RestofMort$Metabolic <- round(MetabolicMort$Deaths + (MetabolicMort$Deaths * IllMort$ShareIll),0)
RestofMort$Resp <- round(RespMort$Deaths + (RespMort$Deaths * IllMort$ShareIll),0)
RestofMort$External <- round(ExternalMort$Deaths + (ExternalMort$Deaths * IllMort$ShareIll),0)
RestofMort$Nerv <- round(NervMort$Deaths + (NervMort$Deaths * IllMort$ShareIll),0)
RestofMort$Rest <- (RestofMort$All - RestofMort$CVD - RestofMort$Cancer - 
                       RestofMort$Metabolic - RestofMort$Resp - RestofMort$Nerv - RestofMort$External)

RestofMort$Age_Groups <-  c(rep(seq(50, 95, by = 5),length.out=900))

RestofMort$CVDShare <- RestofMort$CVD/RestofMort$All
RestofMort$CancerShare <- RestofMort$Cancer/RestofMort$All
RestofMort$MetabolicShare <- RestofMort$Metabolic/RestofMort$All
RestofMort$RespShare <- RestofMort$Resp/RestofMort$All
RestofMort$NervShare <- RestofMort$Nerv/RestofMort$All
RestofMort$ExternalShare <- RestofMort$External/RestofMort$All
RestofMort$RestShare <- RestofMort$Rest/RestofMort$All

ShareofMort <- RestofMort


ShareofMort$Sex[ShareofMort$Sex==1] <- "Male"
ShareofMort$Sex[ShareofMort$Sex==2] <- "Female"

ShareofMortjoin <- ShareofMort %>% 
  select(c(1:4,13:19))


#Canada only reports cause of death data until age 85 (deaths are in the Rest file)
#so we assume that deaths after 90 have the same proportion as 85

# Assuming data frame is called 'ShareofMortjoin'

for (i in c(209, 210, 219, 220, 229, 230, 239, 240, 249, 250, 259, 260, 269, 270, 279, 280, 289, 290, 299, 300)) {
  if (any(is.na(ShareofMortjoin[i, 5:10]))) {
    ShareofMortjoin[i, 5:11] <- ShareofMortjoin[i - 1, 5:11]
  }
}


