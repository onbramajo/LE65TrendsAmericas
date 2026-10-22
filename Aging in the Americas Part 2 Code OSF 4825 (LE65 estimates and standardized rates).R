library(tidyverse)
library(DemoDecomp)
# after deaths, share by cause and population have been uploaded

# part 2 of code: death rates and life tables for LE65. we use deaths from UNWPP but proportion from WHO 

colnames(DeathData)[colnames(DeathData) == "Age"] <- "Age_Groups"


ShareofMortjoin$Country[ShareofMortjoin$Country=="United States of America"] <- "USA"

AllPop$Country[AllPop$Country=="United States of America"] <- "USA"

AllData <- left_join(DeathData,ShareofMortjoin,by=c("Country","Year","Sex","Age_Groups"))

AllData <- AllData %>% 
  filter(Age_Groups <100 )

AllData <- left_join(AllData,AllPop,by=c("Country","Year","Sex","Age_Groups"))

AllData$Deaths <- AllData$mx*AllData$Pop
AllData$Deaths <- round(AllData$Deaths,0)

#join by Country Year Sex    Age_Groups

# get standardized rates, which will be the average proportion for the period

str(AllData)

sum(is.na(AllData$Pop))

StandardizedVec <- AllData %>%
  group_by(Age_Groups) %>% 
  summarize(Standvec=sum(Pop))

print(StandardizedVec)

StandardizedVec$Weights <-StandardizedVec$Standvec/sum(StandardizedVec$Standvec)

AllData$Weights <- rep(StandardizedVec$Weights, length.out= 630)  

StandCVDRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*CVDShare*Weights*100000/Pop))

StandCancerRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*CancerShare*Weights*100000/Pop))

StandExternalRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*ExternalShare*Weights*100000/Pop))

StandMetabolicRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*MetabolicShare*Weights*100000/Pop))

StandRespRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*RespShare*Weights*100000/Pop))

StandRestDeathRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*RestShare*Weights*100000/Pop))

StandNervDeathRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*NervShare*Weights*100000/Pop))

StandAllDeathRate <- AllData %>% 
  group_by(Country,Year,Sex) %>% 
  summarize(StandRate=sum(Deaths*Weights*100000/Pop))

StandRatesforPlot <- rbind(StandCVDRate,StandCancerRate,StandMetabolicRate,StandRespRate,StandExternalRate,StandNervDeathRate,StandRestDeathRate)


StandRatesforPlot$Type <- c(rep("CVD", 90), rep("Cancer", 90),rep("Metabolic", 90),rep("Respiratory", 90),rep("External", 90),rep("Nervous", 90), rep("Rest", 90))




StandRatesforPlot$Country[StandRatesforPlot$Country=="USA"] <- "U.S."


StandRatesforPlot$Country <- factor(StandRatesforPlot$Country, 
                                    levels = c("U.S.","Canada","Argentina", "Brazil", "Chile", "Colombia","Costa Rica", "Mexico", "Peru"))

StandRatesforPlot$Type<- factor(StandRatesforPlot$Type, 
                                levels = c("CVD", "Rest", "Cancer", "Respiratory", "Metabolic","Nervous", "External"))

StandRatesforPlot$Sex<- factor(StandRatesforPlot$Sex, 
                               levels = c("Male", "Female"))




StandRatesforPlot1 <- StandRatesforPlot %>% 
  filter(Type=="Cancer"|Type=="CVD"|Type=="Rest")

StandRatesforPlot2 <- StandRatesforPlot %>% 
  filter(Type=="External"|Type=="Metabolic"|Type=="Respiratory"|Type=="Nervous")


# Calculate the average StandRate for each Type and Sex
average_standrate <- StandRatesforPlot %>%
  filter(Year == "2019") %>% 
  group_by(Type, Sex) %>%
  summarise(avg_standrate = mean(StandRate, na.rm = TRUE))

average_standrate1 <- StandRatesforPlot1 %>%
  filter(Year == "2019") %>% 
  group_by(Type, Sex) %>%
  summarise(avg_standrate = mean(StandRate, na.rm = TRUE))
average_standrate2 <- StandRatesforPlot2 %>%
  filter(Year == "2019") %>% 
  group_by(Type, Sex) %>%
  summarise(avg_standrate = mean(StandRate, na.rm = TRUE))




library(gridExtra)
library(viridis)  # For colorblind-friendly palette

# Common theme for all plots
common_theme <- theme_bw() +
  theme(
    axis.text.y = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    strip.text = element_text(size = 26, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size = 1.5)
  )




# Only plotting Cancer CVD and Rest 

# Define a common y-axis limit
y_limits <- c(0, 4000)
y_limits2 <- c(0, 2500)

# do a new limits scale because they are lower

# for all 7 causes (color)

# for 3 main causes black white

PlotBW1Americas <- ggplot(StandRatesforPlot1, aes(x = as.factor(Year), y = StandRate, group = Country, 
                               color = Country, shape = Country, linetype = Country)) +
  geom_line(size = 1) +
  geom_point(size = 5, stroke = 1.5) +
  scale_shape_manual(values = c(15,16,17,18,19,20,21,22,23),
                     name = "Country") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", 
                                   "longdash", "twodash", "dotdash",
                                   "F1", "4C88C488", "12345678"),
                        name = "Country") +
  scale_color_grey(start = 0.2, end = 0.8) +
  geom_hline(data = filter(average_standrate1), 
             aes(yintercept = avg_standrate), linetype = "solid", 
             color = "black", size = 1.5) +
  scale_y_continuous(limits = y_limits) +
  labs(x = "Year", y = "Standardized Rate") +
  facet_grid(~ Sex ~  Type) +
  theme(
    # Increase font size for axis titles
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Increase font size for axis text
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    
    # Increase font size for facet labels
    strip.text = element_text(size = 16, face = "bold"),
    
    # Remove grey background and modify grid lines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    
    # Increase legend text size
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(2, "cm"),
    
    # Remove strip background (grey from facets)
    strip.background = element_rect(fill = "white"),
    
    # Add border to the plot
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

ggsave("PlotBW1Americas.png", 
       plot = last_plot(),  # or assign your plot to a variable and use that
       width = 14,          # adjust width as needed
       height = 8,          # adjust height as needed
       units = "in",
       dpi = 600,           # 600 DPI for black & white as requested
       bg = "white")        # ensures white background




# other 4 in black and white
PlotBW2Americas <- ggplot(StandRatesforPlot2, aes(x = as.factor(Year), y = StandRate, group = Country, 
                               color = Country, shape = Country, linetype = Country)) +
  geom_line(size = 1) +
  geom_point(size = 5, stroke = 1.5) +
  # Adjust shape scale for black-and-white-friendly shapes
  scale_shape_manual(values = c(15,16,17,18,19,20,21,22,23),
                     name = "Country") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", 
                                   "longdash", "twodash", "dotdash",
                                   "F1", "4C88C488", "12345678"),
                        name = "Country") +
  scale_color_grey(start = 0.2, end = 0.8) +
  # Remove color and use black for all lines
  scale_color_manual(values = rep("black", length(unique(StandRatesforPlot2$Country))),
                     name = "Country") +
  geom_hline(data = filter(average_standrate2), 
             aes(yintercept = avg_standrate), linetype = "solid", 
             color = "black", size = 2) +
  scale_y_continuous(limits = y_limits) +
  labs(x = "Year", y = "Standardized Rate") +
  facet_grid(~ Sex ~ Type) +
  theme(
    # Increase font size for axis titles
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    
    # Increase font size for axis text
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    
    # Increase font size for facet labels
    strip.text = element_text(size = 16, face = "bold"),
    
    # Remove grey background and modify grid lines
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    
    # Increase legend text size
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(2, "cm"),
    
    # Remove strip background (grey from facets)
    strip.background = element_rect(fill = "white"),
    
    # Add border to the plot
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )



ggsave("PlotBW2Americas.png", 
       plot = last_plot(),  # or assign your plot to a variable and use that
       width = 14,          # adjust width as needed
       height = 8,          # adjust height as needed
       units = "in",
       dpi = 600,           # 600 DPI for black & white as requested
       bg = "white")        # ensures white background


# 

# lifetable function
# 
 lifetable <- function(deaths,pop,Age){
   nmx <- deaths/pop
   nax <- 2.5
   n <- c(diff(Age),nax[length(nax)])
   nqx <- n*nmx / (1+(n-nax)*nmx)
   nqx[7] <- 1
   npx <- 1 - nqx
   
  lx <- rep(NA,length(nqx))
   lx[1] <- 100000
   for(i in 1:length(n)){
     lx[i+1] <- lx[i]*npx[i]
   }
   
   lx <- lx[1:length(n)]
   
   ndx <- lx*nqx
   ndx[7] <- lx[7]
   
   
   nLx <- ndx/nmx
   
   # Tx
   
   Tx = rev(cumsum(rev(nLx)))
   
   # remaining life expectancy
   ex <- Tx/lx
   
   lifetable <- data.frame(Age=Age,n=round(n,0),nmx=round(nmx,5),
                           nqx=round(nqx,5),
                           npx=round(npx,5),nax=round(nax,2),
                           ndx=round(ndx,0),lx=round(lx,0),
                           Lx=round(nLx,0),
                           Tx=round(Tx,0),
                          ex=round(ex,2))
   return(lifetable)
 }
 



ARGMale2000 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Male",Year=="2000")
ARGMale2010 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Male",Year=="2005")
ARGMale2010 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Male",Year=="2010")
ARGMale2015 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Male",Year=="2015")
ARGMale2019 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Male",Year=="2019")

ARGFemale2000 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Female",Year=="2000")
ARGFemale2010 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Female",Year=="2005")
ARGFemale2010 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Female",Year=="2010")
ARGFemale2015 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Female",Year=="2015")
ARGFemale2019 <- AllData %>% 
  filter(Country=="Argentina",Sex=="Female",Year=="2019")

BRAMale2000 <- AllData %>% 
  filter(Country=="Brazil",Sex=="Male",Year=="2000")
BRAMale2010 <- AllData %>% 
  filter(Country=="Brazil",Sex=="Male",Year=="2010")
BRAMale2019 <- AllData %>% 
  filter(Country=="Brazil",Sex=="Male",Year=="2019")

BRAFemale2000 <- AllData %>% 
  filter(Country=="Brazil",Sex=="Female",Year=="2000")
BRAFemale2010 <- AllData %>% 
  filter(Country=="Brazil",Sex=="Female",Year=="2010")
BRAFemale2019 <- AllData %>% 
  filter(Country=="Brazil",Sex=="Female",Year=="2019")

CANMale2000 <- AllData %>% 
  filter(Country=="Canada",Sex=="Male",Year=="2000")
CANMale2010 <- AllData %>% 
  filter(Country=="Canada",Sex=="Male",Year=="2010")
CANMale2019 <- AllData %>% 
  filter(Country=="Canada",Sex=="Male",Year=="2019")

CANFemale2000 <- AllData %>% 
  filter(Country=="Canada",Sex=="Female",Year=="2000")
CANFemale2010 <- AllData %>% 
  filter(Country=="Canada",Sex=="Female",Year=="2010")
CANFemale2019 <- AllData %>% 
  filter(Country=="Canada",Sex=="Female",Year=="2019")


CHIMale2000 <- AllData %>% 
  filter(Country=="Chile",Sex=="Male",Year=="2000")
CHIMale2010 <- AllData %>% 
  filter(Country=="Chile",Sex=="Male",Year=="2010")
CHIMale2019 <- AllData %>% 
  filter(Country=="Chile",Sex=="Male",Year=="2019")

CHIFemale2000 <- AllData %>% 
  filter(Country=="Chile",Sex=="Female",Year=="2000")
CHIFemale2010 <- AllData %>% 
  filter(Country=="Chile",Sex=="Female",Year=="2010")

CHIFemale2019 <- AllData %>% 
  filter(Country=="Chile",Sex=="Female",Year=="2019")

COLMale2000 <- AllData %>% 
  filter(Country=="Colombia",Sex=="Male",Year=="2000")
COLMale2010 <- AllData %>% 
  filter(Country=="Colombia",Sex=="Male",Year=="2010")
COLMale2019 <- AllData %>% 
  filter(Country=="Colombia",Sex=="Male",Year=="2019")

COLFemale2000 <- AllData %>% 
  filter(Country=="Colombia",Sex=="Female",Year=="2000")
COLFemale2010 <- AllData %>% 
  filter(Country=="Colombia",Sex=="Female",Year=="2010")
COLFemale2019 <- AllData %>% 
  filter(Country=="Colombia",Sex=="Female",Year=="2019")

CRIMale2000 <- AllData %>% 
  filter(Country=="Costa Rica",Sex=="Male",Year=="2000")
CRIMale2010 <- AllData %>% 
  filter(Country=="Costa Rica",Sex=="Male",Year=="2010")
CRIMale2019 <- AllData %>% 
  filter(Country=="Costa Rica",Sex=="Male",Year=="2019")

CRIFemale2000 <- AllData %>% 
  filter(Country=="Costa Rica",Sex=="Female",Year=="2000")
CRIFemale2010 <- AllData %>% 
  filter(Country=="Costa Rica",Sex=="Female",Year=="2010")
CRIFemale2019 <- AllData %>% 
  filter(Country=="Costa Rica",Sex=="Female",Year=="2019")

MEXMale2000 <- AllData %>% 
  filter(Country=="Mexico",Sex=="Male",Year=="2000")
MEXMale2010 <- AllData %>% 
  filter(Country=="Mexico",Sex=="Male",Year=="2010")
MEXMale2019 <- AllData %>% 
  filter(Country=="Mexico",Sex=="Male",Year=="2019")

MEXFemale2000 <- AllData %>% 
  filter(Country=="Mexico",Sex=="Female",Year=="2000")
MEXFemale2010 <- AllData %>% 
  filter(Country=="Mexico",Sex=="Female",Year=="2010")
MEXFemale2019 <- AllData %>% 
  filter(Country=="Mexico",Sex=="Female",Year=="2019")

PERMale2000 <- AllData %>% 
  filter(Country=="Peru",Sex=="Male",Year=="2000")
PERMale2010 <- AllData %>% 
  filter(Country=="Peru",Sex=="Male",Year=="2010")
PERMale2019 <- AllData %>% 
  filter(Country=="Peru",Sex=="Male",Year=="2019")

PERFemale2000 <- AllData %>% 
  filter(Country=="Peru",Sex=="Female",Year=="2000")
PERFemale2010 <- AllData %>% 
  filter(Country=="Peru",Sex=="Female",Year=="2010")
PERFemale2019 <- AllData %>% 
  filter(Country=="Peru",Sex=="Female",Year=="2019")

USAMale2000 <- AllData %>% 
  filter(Country=="USA",Sex=="Male",Year=="2000")
USAMale2010 <- AllData %>% 
  filter(Country=="USA",Sex=="Male",Year=="2010")
USAMale2019 <- AllData %>% 
  filter(Country=="USA",Sex=="Male",Year=="2019")

USAFemale2000 <- AllData %>% 
  filter(Country=="USA",Sex=="Female",Year=="2000")
USAFemale2010 <- AllData %>% 
  filter(Country=="USA",Sex=="Female",Year=="2010")
USAFemale2019 <- AllData %>% 
  filter(Country=="USA",Sex=="Female",Year=="2019")

# don't forget to summarize!   remove US and Canada for AVG Latam



AVGMale2000 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada"), 
         Sex == "Male", 
         Year == "2000") %>% 
  group_by(Year,Age_Groups,Sex) %>%
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGMale2000$Country <- "Average"

AVGMale2010 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada"), 
         Sex == "Male", 
         Year == "2010") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))
AVGMale2010$Country <- "Average"

AVGMale2019 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada"), 
         Sex == "Male", 
         Year == "2019") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))
AVGMale2019$Country <- "Average"


AVGFemale2000 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada"), 
         Sex == "Female", 
         Year == "2000") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGFemale2000$Country <- "Average"


AVGFemale2010 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada"), 
         Sex == "Female", 
         Year == "2010") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGFemale2010$Country <- "Average"

AVGFemale2019 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada"), 
         Sex == "Female", 
         Year == "2019") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGFemale2019$Country <- "Average"

# Average w/o Mexico without Mexico due to stalling

AVGnoMexMale2000 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada","Mexico"), 
         Sex == "Male", 
         Year == "2000") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGnoMexMale2000$Country <- "Average w/o Mexico"

AVGnoMexMale2010 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada","Mexico"), 
         Sex == "Male", 
         Year == "2010") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGnoMexMale2010$Country <- "Average w/o Mexico"

AVGnoMexMale2019 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada","Mexico"), 
         Sex == "Male", 
         Year == "2019") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGnoMexMale2019$Country <- "Average w/o Mexico"


AVGnoMexFemale2000 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada","Mexico"), 
         Sex == "Female", 
         Year == "2000") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGnoMexFemale2000$Country <- "Average w/o Mexico"


AVGnoMexFemale2010 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada","Mexico"), 
         Sex == "Female", 
         Year == "2010") %>%  
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGnoMexFemale2010$Country <- "Average w/o Mexico"

AVGnoMexFemale2019 <- AllData %>% 
  filter(!Country %in% c("USA", "Canada","Mexico"), 
         Sex == "Female", 
         Year == "2019") %>% 
  group_by(Year,Age_Groups,Sex) %>% 
  mutate(   CVDDeaths=Deaths*(CVDShare),
            CancerDeaths=Deaths*(CancerShare),
            MetabolicDeaths=Deaths*(MetabolicShare),
            RespDeaths=Deaths*(RespShare),
            ExternalDeaths=Deaths*(ExternalShare),
            NervDeaths=Deaths*(NervShare),
            RestDeaths=Deaths*(RestShare)) %>% 
  summarize(Deaths=sum(Deaths),
            CVDShare=sum(CVDDeaths)/sum(Deaths),
            CancerShare=sum(CancerDeaths)/sum(Deaths),
            MetabolicShare=sum(MetabolicDeaths)/sum(Deaths),
            RespShare=sum(RespDeaths)/sum(Deaths),
            ExternalShare=sum(ExternalDeaths)/sum(Deaths), 
            NervShare=sum(NervDeaths)/sum(Deaths),
            RestShare=sum(RestDeaths)/sum(Deaths),
            Pop=sum(Pop))

AVGnoMexFemale2019$Country <- "Average w/o Mexico"

# summary LT
 
# 
# 
 LtAllAVGMale2000 <- lifetable(AVGMale2000$Deaths,AVGMale2000$Pop,AVGMale2000$Age_Groups)
 
 LtAllAVGFemale2000 <- lifetable(AVGFemale2000$Deaths,AVGFemale2000$Pop,AVGFemale2000$Age_Groups)
 
 
 LtAllAVGMale2010 <- lifetable(AVGMale2010$Deaths,AVGMale2010$Pop,AVGMale2010$Age_Groups)
 
 LtAllAVGFemale2010 <- lifetable(AVGFemale2010$Deaths,AVGFemale2010$Pop,AVGFemale2010$Age_Groups)
# 
 
 LtAllAVGMale2019 <- lifetable(AVGMale2019$Deaths,AVGMale2019$Pop,AVGMale2019$Age_Groups)
 
 LtAllAVGFemale2019 <- lifetable(AVGFemale2019$Deaths,AVGFemale2019$Pop,AVGFemale2019$Age_Groups)
 
 
 LtAllAVGnoMexMale2000 <- lifetable(AVGnoMexMale2000$Deaths,AVGnoMexMale2000$Pop,AVGnoMexMale2000$Age_Groups)
 
 LtAllAVGnoMexFemale2000 <- lifetable(AVGnoMexFemale2000$Deaths,AVGnoMexFemale2000$Pop,AVGnoMexFemale2000$Age_Groups)
 
 
 LtAllAVGnoMexMale2010 <- lifetable(AVGnoMexMale2010$Deaths,AVGnoMexMale2010$Pop,AVGnoMexMale2010$Age_Groups)
 
 LtAllAVGnoMexFemale2010 <- lifetable(AVGnoMexFemale2010$Deaths,AVGnoMexFemale2010$Pop,AVGnoMexFemale2010$Age_Groups)
# 
# 
 LtAllAVGnoMexMale2019 <- lifetable(AVGnoMexMale2019$Deaths,AVGnoMexMale2019$Pop,AVGnoMexMale2019$Age_Groups)
# 
 LtAllAVGnoMexFemale2019 <- lifetable(AVGnoMexFemale2019$Deaths,AVGnoMexFemale2019$Pop,AVGnoMexFemale2019$Age_Groups)

# write.table(AllData,"AllDeathsandpopbycauseamericas20002019.xls",sep="/")

# summary LT


ARGLE <- c(ARGMale2000$ex[1],ARGMale2010$ex[1],ARGMale2019$ex[1],
           ARGFemale2000$ex[1],ARGFemale2010$ex[1],ARGFemale2019$ex[1])

BRALE <-c(BRAMale2000$ex[1],BRAMale2010$ex[1],BRAMale2019$ex[1],
          BRAFemale2000$ex[1],BRAFemale2010$ex[1],BRAFemale2019$ex[1])

CANLE <- c(CANMale2000$ex[1],CANMale2010$ex[1],CANMale2019$ex[1],
           CANFemale2000$ex[1],CANFemale2010$ex[1],CANFemale2019$ex[1])

CHILE <- c(CHIMale2000$ex[1],CHIMale2010$ex[1],CHIMale2019$ex[1],
           CHIFemale2000$ex[1],CHIFemale2010$ex[1],CHIFemale2019$ex[1])

COLLE <-c(COLMale2000$ex[1],COLMale2010$ex[1],COLMale2019$ex[1],
          COLFemale2000$ex[1],COLFemale2010$ex[1],COLFemale2019$ex[1])

CRILE <-c(CRIMale2000$ex[1],CRIMale2010$ex[1],CRIMale2019$ex[1],
          CRIFemale2000$ex[1],CRIFemale2010$ex[1],CRIFemale2019$ex[1])

PERLE <-c(PERMale2000$ex[1],PERMale2010$ex[1],PERMale2019$ex[1],
          PERFemale2000$ex[1],PERFemale2010$ex[1],PERFemale2019$ex[1])

MEXLE <-c(MEXMale2000$ex[1],MEXMale2010$ex[1],MEXMale2019$ex[1],
          MEXFemale2000$ex[1],MEXFemale2010$ex[1],MEXFemale2019$ex[1])

USALE <-c(USAMale2000$ex[1],USAMale2010$ex[1],USAMale2019$ex[1],
          USAFemale2000$ex[1],USAFemale2010$ex[1],USAFemale2019$ex[1])

AVGLE <-c(LtAllAVGMale2000$ex[1],LtAllAVGMale2010$ex[1],LtAllAVGMale2019$ex[1],
          LtAllAVGFemale2000$ex[1],LtAllAVGFemale2010$ex[1],LtAllAVGFemale2019$ex[1])

AVGnoMexLE <-c(LtAllAVGnoMexMale2000$ex[1],LtAllAVGnoMexMale2010$ex[1],LtAllAVGnoMexMale2019$ex[1],
               LtAllAVGnoMexFemale2000$ex[1],LtAllAVGnoMexFemale2010$ex[1],LtAllAVGnoMexFemale2019$ex[1])

ValueLE <- c(ARGLE,BRALE,CANLE,CHILE,COLLE,CRILE,MEXLE,PERLE,USALE,AVGLE,AVGnoMexLE)

ValueLE <- round(ValueLE,2)

t(ValueLE)

# Create Year vector
SexVec <- c(rep(c("Male", "Male", "Male", "Female", "Female", "Female"), times = 11))

# Create Year vector
# Create Year vector
YearVec <- rep(c(2000, 2010, 2019), length.out=66)

# Create Value vector
ValueVec <- c(ARGLE, BRALE, CANLE, CHILE, COLLE, CRILE, MEXLE, PERLE, USALE, AVGLE, AVGnoMexLE)

CountryVec <- c(rep(c("Argentina", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Mexico", "Peru", "U.S.", "Latin America","Latin America w/o Mexico"), each = 6))

length(SexVec)
length(YearVec)
length(CountryVec)
length(ValueVec)

# Create dataframe
ALLLES <- data.frame(SexVec, YearVec, CountryVec, ValueVec)

# Rename the column names
colnames(ALLLES) <- c("Sex", "Year", "Country", "Value")


 library(ggplot2)
 library(dplyr)
 library(ggrepel)

 library(forcats)
# 
# DataforPlotFromTable2 <- DataforPlotFromTable2 %>%
#   mutate(Country = fct_rev(Country))

str(ALLLES)

ALLLES$Year <- as.factor(ALLLES$Year)



ALLLESwoMEX <- ALLLES %>% 
  filter(Country!="Latin America w/o Mexico")




ALLLESwoMEX$Country <- factor(ALLLESwoMEX$Country, 
                                    levels = c("U.S.","Canada","Latin America","Argentina", "Brazil", "Chile", "Colombia","Costa Rica", "Mexico", "Peru"))



ALLLESwoMEX$Country <- factor(ALLLESwoMEX$Country, 
                              levels = c("Peru","Mexico","Costa Rica","Colombia", "Chile", "Brazil", "Argentina","Latin America", "Canada", "U.S."))


rev(ALLLESwoMEX$Country)

# figure 1


Plot1LEAmericas <- ggplot(ALLLESwoMEX, aes(x = Value, y = Country, color = Year, shape = Year)) +
  geom_point(size = 7, alpha = 0.5, stroke = 1) +
  geom_text_repel(aes(label = Value), 
                  nudge_y = 0.25, 
                  size = 7, 
                  fontface = "bold",
                  show.legend = FALSE) +
  facet_grid(~ Sex, scales = "free_x") +
  # Grayscale color scale
  scale_color_grey(start = 0.2, end = 0.8, 
                   breaks = c("2000", "2010", "2019")) +
  scale_shape_manual(values = c(15, 17, 19),
                     breaks = c("2000", "2010", "2019")) +
  labs(x = "Value", y = "Country", title = " ") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    strip.text = element_text(size = 20, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 5)),
         shape = guide_legend(override.aes = list(size = 5)))

ggsave("Plot1LEAmericas.png", 
       plot = last_plot(),  # or assign your plot to a variable and use that
       width = 12,          # adjust width as needed
       height = 8,          # adjust height as needed
       units = "in",
       dpi = 600,           # 600 DPI for black & white as requested
       bg = "white")        # ensures white background


# estimate coefficients of variation CV

# Object with U.S., Canada, and Latin America

ALLLESwideLatam<- ALLLESwoMEX %>% 
  pivot_wider(values_from=Value,names_from=c(Year,Sex)) %>% 
  filter(Country %in% c("U.S.", "Canada", "Latin America"))

cv_2000_MaleLatam <- sd(ALLLESwideLatam$`2000_Male`) / mean(ALLLESwideLatam$`2000_Male`) * 100
cv_2010_MaleLatam <- sd(ALLLESwideLatam$`2010_Male`) / mean(ALLLESwideLatam$`2010_Male`) * 100
cv_2019_MaleLatam <- sd(ALLLESwideLatam$`2019_Male`) / mean(ALLLESwideLatam$`2019_Male`) * 100
cv_2000_FemaleLatam <- sd(ALLLESwideLatam$`2000_Female`) / mean(ALLLESwideLatam$`2000_Female`) * 100
cv_2010_FemaleLatam <- sd(ALLLESwideLatam$`2010_Female`) / mean(ALLLESwideLatam$`2010_Female`) * 100
cv_2019_FemaleLatam <- sd(ALLLESwideLatam$`2019_Female`) / mean(ALLLESwideLatam$`2019_Female`) * 100

print(cv_2000_MaleLatam)
print(cv_2010_MaleLatam)
print(cv_2019_MaleLatam)
print(cv_2000_FemaleLatam)
print(cv_2010_FemaleLatam)
print(cv_2019_FemaleLatam)

  
# Object without Latin America
ALLLESwidenoLatam<- ALLLESwoMEX %>% 
  pivot_wider(values_from=Value,names_from=c(Year,Sex)) %>% 
  filter(!Country %in% c("Latin America"))


cv_2000_MalenoLatam <- sd(ALLLESwidenoLatam$`2000_Male`) / mean(ALLLESwidenoLatam$`2000_Male`) * 100
cv_2010_MalenoLatam <- sd(ALLLESwidenoLatam$`2010_Male`) / mean(ALLLESwidenoLatam$`2010_Male`) * 100
cv_2019_MalenoLatam <- sd(ALLLESwidenoLatam$`2019_Male`) / mean(ALLLESwidenoLatam$`2019_Male`) * 100
cv_2000_FemalenoLatam <- sd(ALLLESwidenoLatam$`2000_Female`) / mean(ALLLESwidenoLatam$`2000_Female`) * 100
cv_2010_FemalenoLatam <- sd(ALLLESwidenoLatam$`2010_Female`) / mean(ALLLESwidenoLatam$`2010_Female`) * 100
cv_2019_FemalenoLatam <- sd(ALLLESwidenoLatam$`2019_Female`) / mean(ALLLESwidenoLatam$`2019_Female`) * 100

print(cv_2000_MalenoLatam)
print(cv_2010_MalenoLatam)
print(cv_2019_MalenoLatam)
print(cv_2000_FemalenoLatam)
print(cv_2010_FemalenoLatam)
print(cv_2019_FemalenoLatam)

