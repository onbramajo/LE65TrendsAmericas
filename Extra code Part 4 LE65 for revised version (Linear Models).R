

library(readr)

# set working directory or route

LE65bySex <- read_delim("route/LE65bySextxt.txt", 
                        delim = "\t", escape_double = FALSE,
                        trim_ws = TRUE)


colnames(LE65bySex)

LE65bySex <- LE65bySex %>% 
  select(1,2,7:12,10:19)


LE65forDeaths5years <- LE65bySex %>% 
    filter(Year=="2000"|Year=="2005"|Year=="2010"|Year=="2015"|Year=="2019",Age < 100)


OnlyLE65forModel <- LE65bySex %>% 
  filter(Age=="65")

str(OnlyLE65forModel)


OnlyLE65forModel<- OnlyLE65forModel %>%
  mutate(
    Year_centered = Year - 2010,
    Year_after_2010 = pmax(0, Year - 2010)
  )

OnlyLE65forModel

ggplot(OnlyLE65forModel, aes(x = Year, y = ex, color=Country)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Sex) +
  theme_bw() +
  labs(title = "Life Expectancy at Age 65 by Year and Country",
       x = "Year",
       y = "Life Expectancy (ex)") 

# stratify country and sex for each linear regression model

MaleArgentinaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Argentina")
FemaleArgentinaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Argentina")
MaleBrazilModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Brazil")
FemaleBrazilModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Brazil")
MaleChileModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Chile")
FemaleChileModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Chile")
MaleCostaricaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Costa Rica")
FemaleCostaricaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Costa Rica")
MaleColombiaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Colombia")
FemaleColombiaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Colombia")
MaleMexicoModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Mexico")
FemaleMexicoModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Mexico")
MalePeruModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Peru")
FemalePeruModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Peru")

MaleLatinAmericaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country!="Canada",Country!="USA")
FemaleLatinAmericaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country!="Canada",Country!="USA")


MaleCanadaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="Canada")
FemaleCanadaModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="Canada")

MaleUSAModel <- OnlyLE65forModel %>% 
  filter(Sex=="Male",Country=="USA")
FemaleUSAModel <- OnlyLE65forModel %>% 
  filter(Sex=="Female",Country=="USA")

# to compare with HMD

CanChiUSA <- OnlyLE65forModel %>% 
  filter(Country=="USA"|Country=="Canada"|Country=="Chile")


model11 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleArgentinaModel
)
summary(model11)

model12 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleBrazilModel
)
summary(model12)

model13 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleChileModel
)
summary(model13)

model14 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleCostaricaModel
)
summary(model14)

model15 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleColombiaModel
)
summary(model15)

model16 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleMexicoModel
)
summary(model16)

model17 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MalePeruModel
)
summary(model17)

model18 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleCanadaModel
)
summary(model18)

model19 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleUSAModel
)
summary(model19)

model10 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = MaleLatinAmericaModel
)
summary(model10)



model21 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleArgentinaModel
)
summary(model21)


model29 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleUSAModel
)
summary(model29)



model21 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleArgentinaModel
)
summary(model21)

model22 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleBrazilModel
)
summary(model22)

model23 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleChileModel
)
summary(model23)

model24 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleCostaricaModel
)
summary(model24)

model25 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleColombiaModel
)
summary(model25)

model26 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleMexicoModel
)
summary(model26)

model27 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemalePeruModel
)
summary(model27)

model28 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleCanadaModel
)
summary(model28)

model29 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleUSAModel
)
summary(model29)

model20 <- glm(
  ex ~ Year_centered + Year_after_2010,
  family= gaussian(link = "identity"),
  data = FemaleLatinAmericaModel
)
summary(model20)

summary_with_ci11 <- cbind(
  coef(summary(model11)), 
  confint(model11))

summary_with_ci12 <- cbind(
  coef(summary(model12)), 
  confint(model12))


summary_with_ci13 <- cbind(
  coef(summary(model13)), 
  confint(model13))

summary_with_ci14 <- cbind(
  coef(summary(model14)), 
  confint(model14))

summary_with_ci15 <- cbind(
  coef(summary(model15)), 
  confint(model15))

summary_with_ci16 <- cbind(
  coef(summary(model16)), 
  confint(model16))

summary_with_ci17 <- cbind(
  coef(summary(model17)), 
  confint(model17))

summary_with_ci18 <- cbind(
  coef(summary(model18)), 
  confint(model18))

summary_with_ci19 <- cbind(
  coef(summary(model19)), 
  confint(model19))

summary_with_ci10 <- cbind(
  coef(summary(model10)), 
  confint(model10))

summary_with_ci21 <- cbind(
  coef(summary(model21)), 
  confint(model21))

summary_with_ci22 <- cbind(
  coef(summary(model22)), 
  confint(model22))


summary_with_ci23 <- cbind(
  coef(summary(model23)), 
  confint(model23))

summary_with_ci24 <- cbind(
  coef(summary(model24)), 
  confint(model24))

summary_with_ci25 <- cbind(
  coef(summary(model25)), 
  confint(model25))

summary_with_ci26 <- cbind(
  coef(summary(model26)), 
  confint(model26))

summary_with_ci27 <- cbind(
  coef(summary(model27)), 
  confint(model27))

summary_with_ci28 <- cbind(
  coef(summary(model28)), 
  confint(model28))

summary_with_ci29 <- cbind(
  coef(summary(model29)), 
  confint(model29))

summary_with_ci20 <- cbind(
  coef(summary(model20)), 
  confint(model20))


summary_with_ci11 #Argentina
summary_with_ci12 #Brazil
summary_with_ci13 #Chile
summary_with_ci14 #Costa Rica
summary_with_ci15 #Colombia
summary_with_ci16 #Mexico
summary_with_ci17 #Peru
summary_with_ci18 #Canada
summary_with_ci19 #US
summary_with_ci10 #Latin America 


summary_with_ci21 #Argentina
summary_with_ci22 #Brazil
summary_with_ci23 #Chile
summary_with_ci24 #Costa Rica
summary_with_ci25 #Colombia
summary_with_ci26 #Mexico
summary_with_ci27 #Peru
summary_with_ci28 #Canada
summary_with_ci29 #US
summary_with_ci20 #Latin America 


summary(model11) #Argentina
summary(model12) #Brazil
summary(model13) #Chile
summary(model14) #Costa Rica
summary(model15) #Colombia
summary(model16) #Mexico
summary(model17) #Peru
summary(model18) #Canada
summary(model19) #US
summary(model10) #Latin America 


summary(model21) #Argentina
summary(model22) #Brazil
summary(model23) #Chile
summary(model24) #Costa Rica
summary(model25) #Colombia
summary(model26) #Mexico
summary(model27) #Peru
summary(model28) #Canada
summary(model29) #US
summary(model20) #Latin America 

###
