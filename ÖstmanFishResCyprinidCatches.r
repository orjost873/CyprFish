
library("readxl")
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)

setwd('...')

#Data 2019####
Data2019 <- read_excel("ÖstmanFishResCatchData.xlsx", sheet = "CPUE_2019")

# Variables as numeric
Data2019$By <- as.numeric(Data2019$By) 
Data2019$P_BY <- as.numeric(Data2019$P_BY) 
Data2019$Roach <- as.numeric(Data2019$Roach) 

str(Data2019)

#Remove data from 1 m fyke-nets
Data2019<-Data2019%>%filter(Gear!="1m")

#Bream
B_cpue <- lmer(log(Bream/Effort+0.1) ~ Gear   + (1|Site/Rep), data = Data2019)
anova(B_cpue)
summary(B_cpue)
AIC(B_cpue)
qqnorm(resid(B_cpue))
qqline(resid(B_cpue))
emmeans(B_cpue, "Gear")

#Post-hoc
B_cpue <- aov(log(Bream/Effort+0.1) ~ Gear, data = Data2019)
TukeyHSD(B_cpue)

#Fig 1a####
ggplot(Data2019, aes(x = Gear, y = Bream/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Bream") + xlab("Area") + ylab("CPUE (kg/day)") +
  theme_bw()+
  theme(plot.title = element_text(size = 10))

#Ide
I_cpue <- glmer.nb(Ide/Effort ~ Gear  + (1|Site/Rep), data = Data2019)
Anova(I_cpue, Statistics = "F")
summary(I_cpue)
qqplot(I_cpue)
qqnorm(resid(I_cpue))
qqline(resid(I_cpue))
I_cpue <- aov(Ide/Effort ~ Gear, data = Data2019)
TukeyHSD(I_cpue)

#Fig 1b####
ggplot(Data2019, aes(x = Gear, y =Ide/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Ide") + xlab("Gear") + ylab("CPUE") +
  theme_bw()+
  theme(plot.title = element_text(size = 10))

#By-catch
By_cpue <- lmer(log(By/Effort+0.1) ~ Gear    + (1|Site/Rep), data = Data2019, na.action=na.omit)
anova(By_cpue)
summary(By_cpue)
emmeans(By_cpue, "Gear")
qqnorm(resid(I_cpue))
qqline(resid(I_cpue))

#Fig 1c####
ggplot(Data2019, aes(x = Gear, y =By/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("By-catch") + xlab("Gear") + ylab("CPUE") +
  theme_bw()+
  theme(plot.title = element_text(size = 10))

#Percent by-catch
P_By_cpue <- lmer(log(By/(By+Bream+Ide)) ~ Gear   + (1|Site/Rep), data = Data2019, na.action=na.omit)
anova(P_By_cpue)
summary(P_By_cpue)
emmeans(P_By_cpue, "Gear")
qqnorm(resid(P_By_cpue))
qqline(resid(P_By_cpue))
P_By_cpue <- aov((By/(By+Bream+Ide)) ~ Gear  * Site, data = Data2019, na.action=na.omit)
TukeyHSD(P_By_cpue)

#Fig 1d####
ggplot(Data2019, aes(x = Gear, y =(By/(Bream+Ide)))) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("By-catch") + xlab("Gear") + ylab("Ratio by-catch/cyprinds") +
  theme_bw()+
  theme(plot.title = element_text(size = 10))

#CPUE indicator 2019####
cpue_bream2019 <- Data2019 %>%
  group_by(Gear, Site, Season)%>%
  summarise(mC=mean(Bream/Effort),
  medianC=median(Bream/Effort),
  C75=quantile(Bream/Effort, probs = 0.75),
  C25=quantile(Bream/Effort, probs = 0.25),
  CV = sd(Bream/Effort)/mean(Bream/Effort))

#2020-2021####
Data <- read_excel("ÖstmanFishResCatchData.xlsx", sheet = "CPUE_2020-2021")
str(Data)
library(scales)
require(zoo)
library(MASS)

#Season 2020
Data2020<-Data%>%filter(Year==2020)

#Bream
Bream_cpue <- lm(log(Bream/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
Anova(Bream_cpue)
summary(Bream_cpue)
qqnorm(resid(Bream_cpue))
qqline(resid(Bream_cpue))
Bream_cpue <- aov(log(Bream/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
TukeyHSD(Bream_cpue, "Season")


#Fig 2a####
Data2020%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Summer", "Fall")))%>%
  ggplot(., aes(x = Season, y =Bream/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Bream") + xlab("Season") + ylab("CPUE") +
  #scale_x_discrete(labels=date_format("%b"))    +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#Ide
I_cpue <- lm(log(Ide/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
anova(I_cpue)
summary(I_cpue)
qqnorm(resid(I_cpue))
qqline(resid(I_cpue))

#Fig 2b####
Data2020%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Summer", "Fall")))%>%
  ggplot(., aes(x = Season, y =Ide/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Ide") + xlab("Season") + ylab("CPUE") +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#Perch
Perch_cpue <- lm(log(Perch/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
anova(Perch_cpue)
summary(Perch_cpue)
qqnorm(resid(Perch_cpue))
qqline(resid(Perch_cpue))

Perch_cpue <- aov(log(Perch/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
TukeyHSD(Perch_cpue, "Season")

#Fig 2c####
Data2020%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Summer", "Fall")))%>%
  ggplot(., aes(x = Season, y =Perch/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Perch") + xlab("Season") + ylab("CPUE") +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#Percent perch-to-cyprinids
P_BY <- lm(log(Perch/(Bream+Ide)+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
Anova(P_BY)
summary(Perch_cpue)
qqnorm(resid(Perch_cpue))
qqline(resid(Perch_cpue))

P_BY <- aov(log(Perch/(Bream+Ide)+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
TukeyHSD(P_BY, "Season")

#Fig 2d####
Data2020%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Summer", "Fall")))%>%
  ggplot(., aes(x = Season, y = Perch/(Bream+Ide))) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Perch/Cyprinids") + xlab("Season") + ylab("Proportion") +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#Pike
Pike_cpue <- lm(log(Pike/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
anova(Pike_cpue)
summary(Pike_cpue)
emmeans(Perch_cpue, "Month")
qqnorm(resid(Pike_cpue))
qqline(resid(Pike_cpue))

#Fig 2e####
Data2020%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Summer", "Fall")))%>%
  ggplot(., aes(x = Season, y =Pike/Effort)) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Pike") + xlab("Season") + ylab("CPUE") +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#Whitefish
W_cpue <- lm(log(Whitefish/Effort+1) ~ Season  + Site, data = Data2020, na.action=na.omit)
Anova(W_cpue)
summary(W_cpue)
qqnorm(resid(W_cpue))
qqline(resid(W_cpue))

#Fig 2f####
Data2020%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Summer", "Fall")))%>%
  ggplot(., aes(x = Season, y =Whitefish/Effort)) +
  geom_boxplot()+
  #scale_y_continuous(trans='log2')+
  ggtitle("Whitefish") + xlab("Season") + ylab("CPUE") +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#CPUE Indicators 2020-2021####
cpue_bream <- Data %>%
  group_by(Site, Year, Season)%>%
  summarise(N = n(),
            mC=mean(Bream/Effort),
            C25=quantile(Bream/Effort, probs = 0.25),
            medianC=median(Bream/Effort),
            C75=quantile(Bream/Effort, probs = 0.75),
            CV = sd(Bream/Effort)/mean(Bream/Effort))

#Fig 3####
Data%>%filter(Season=="Spring")%>%
  ggplot(., aes(x = Site, y =Bream/Effort, fill = as.factor(Year))) +
  geom_boxplot()+
  scale_y_continuous(trans='log2')+
  ggtitle("Bream") + xlab("Site") + ylab("CPUE (kg/day)") +
  #scale_x_discrete(labels=date_format("%b"))    +
  theme_bw()+
  theme(plot.title = element_text(size = 10))


#Size####
DataL <- read_excel("ÖstmanFishResCatchData.xlsx", sheet = "Length")
str(DataL)

#Season
DataL_Season<-DataL%>%filter(xor(Site=="Råneå" & Year =="2020", Site=="Borgarudden" & Year=="2021"))

L.lm <- lm(Length ~ Season + Site, data = DataL_Season, na.action=na.omit)
Anova(L.lm, test.statistics = "F")
summary(L.lm)
L.lm <- aov(Length ~ Season + Site, data = DataL_Season, na.action=na.omit)
TukeyHSD(L.lm, "Season")

#Fig. 5a
DataL_Season%>%
  mutate(Season=factor(Season, levels = c( "Spring", "Autumn")))%>%
  ggplot(., aes(x = Season, y =Length)) +
  geom_boxplot()+
  ggtitle("A)") + xlab("Season") + ylab("Length (cm)") +
  theme_bw()+
  theme(plot.title = element_text(size = 10))

#Year and site
DataL_Sp<-DataL%>%filter(Season!="Autumn", Site!="Brändöfjärden", Site!="Borgarudden")
L.lm <- lm(Length ~ Site + as.factor(Year), data = DataL_Sp, na.action=na.omit)
Anova(L.lm, test.statistics = "F")
summary(L.lm)

L.lm <- aov(Length ~Site + as.factor(Year), data = DataL_Sp, na.action=na.omit)
TukeyHSD(L.lm, "Site")

#Fig. 5b
ggplot(DataL_Sp, aes(x = Site, y =Length)) +
  geom_boxplot()+
  ggtitle("B)") + xlab("Site") + ylab("Length (cm)") +
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(size = 10))

#Size indicators####
DataL$N <- 1
L_bream <- DataL %>%
  group_by(Year, Site, Season)%>%
  #group_by(Year)%>%
  summarise(mL=mean(Length),
            medianL=median(Length),
            L90=quantile(Length, probs = 0.9),
            L10=quantile(Length, probs = 0.1),
            LFI30 = sum(N[which(Length>=30)]/sum(N)),
            LFI40 = sum(N[which(Length>=40)]/sum(N)),
            Lmega=weighted.mean(Length[which(Length>=40)],N[which(Length>=40)]),
            Lmax=weighted.mean(Length[which(Length>=quantile(rep(Length,N), probs = 0.9))],N[which(Length>=quantile(rep(Length,N), probs = 0.9))]),
            Antal = sum(N),
            L_CV = sd(Length)/mean(Length))


#Age####

DataA_Br <- read_excel("ÖstmanFishResCatchData.xlsx", sheet = "Age")

#DataA_Br<-DataA_Br%>%filter(Age<48)

require(fishmethods)
library(FSAdata) # for data
library(FSA)     # for vbFuns(), vbStarts(), confint.bootCase()

(svTall <- vbStarts(L~Age, data=DataA_Br, type="traditional", plot=T))
(p1.g <- growth(size=DataA_Br$L, age=DataA_Br$Age, Sinf=svTall$Linf, K=svTall$K, t0=svTall$t0))
summary(p1.g$vout)  # Bertalanffy

DataA_Br2<-DataA_Br%>%filter(Age<48)
( svTall <- vbStarts(L~Age, data=DataA_Br2, fixed=list(Linf=600,K=0.2), plot=T) )
(p1.g <- growth(size=DataA_Br2$L, age=DataA_Br2$Age, Sinf=svTall$Linf, K=svTall$K, t0=svTall$t0))
summary(p1.g$vout)  # Bertalanffy

# Bertalanffy
Sinf<-518.17907   
K<-0.11757    
t0<-0.34605  

#Age 49 removed
Sinf2<-514.25974   
K2<-0.12232    
t02<-0.56917   

#Fig 6
DataA_Br%>%
  ggplot(., aes(x=Age, y=L))+
  geom_point(aes(shape=Sex)) +
  geom_smooth(method = "nls", formula = y ~ a * (1 - exp(-(b * (x - c)))), 
              method.args = list(start=c(a=Sinf,b=K,c=t0)), se = FALSE, colour = "blue") +  
  geom_smooth(method = "nls", formula = y ~ a * (1 - exp(-(b * (x - c)))), 
              method.args = list(start=c(a=Sinf2,b=K2,c=t02)), se = FALSE, colour = "red") +  
  labs(x = "Age", y = "Length (mm)")+
  theme_bw()

#Length-at-age
AatL.lm <- DataA_Br%>%filter(Sex!="NA")%>%
  lm(L ~ Age + Sex, data = .)
summary(AatL.lm)
Anova(AatL.lm)

#Mortality####
ZD<-DataA_Br%>%
  group_by(Age)%>%
  summarise(N=n())

Age <- ZD$Age
Age
N <- ZD$N
N

Zr<-chapmanRobson(Age,N,ages2use = 11:30)
summary(Zr)
plot(Zr)

Zr$est


