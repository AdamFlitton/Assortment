##############Load in
setwd("~/desktop/SL Practical")
library(readxl)
dat<-read_excel("AssortmentData_2013-2017.xls",sheet=3)

##############Factorise categorical variable - note new variable for majority/minority males and females. No reason for 
##############this other than simplicity. Let me know what you think.
dat$Condition<-as.factor(dat$Condition)
dat$Sex<-as.factor(dat$Sex)
dat$Year<-as.factor(dat$Year)
dat$RatioMajMin<-ifelse(dat$PropF<=0.49,"min",ifelse(dat$PropF>=0.51,"maj","split"))
dat$RatioMajMin<-as.factor(dat$RatioMajMin)
dat$PropF<-as.factor(dat$PropF)
dat$Priorknowledge<-as.factor(dat$Priorknowledge)
dat$GroupIDforGLMM<-as.factor(dat$GroupIDforGLMM)
levels(dat$Sex)<-c("Male","Female")
levels(dat$Condition)<-c("Random","Assortment")

##############Plot prep
dodge<-position_dodge(width=0.75)
library(ggplot2)

##############Violin plot collapsed across years, interaction between sex and condition.
ggplot(dat,aes(x=Condition,y=Contributiontopotpence,fill=Sex))+
  geom_violin(position=dodge)+
  geom_point(position=position_jitterdodge())+
  #facet_wrap(~Year)+
  theme_classic()

##############As above, but for all years (can make a violin plot here too, but the low number of males in 
##############2014 means that I cannot generate a violin for them, so I've left it as a dotplot)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence,fill=Sex))+
  geom_point(aes(color=Sex),size=2,position=position_jitterdodge())+
  facet_wrap(~Year)+
  theme_classic()


##############Univariate tests
t.test(dat$Contributiontopotpence~dat$Condition)
t.test(dat$Contributiontopotpence~dat$Sex)

##############ANOVAs
av<-aov(dat$Contributiontopotpence~dat$RatioMajMin)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$Sex)
summary(av)
TukeyHSD(av)

#############GLMMs
library(lme4)

#############Before running models, how much variation is attributable to years?
icc1<-lmer(dat$Contributiontopotpence~(1|dat$Year))
summary(icc1)
#############5% of variation at level of year
############Just out of interest, how much variation between groups?
icc2<-lmer(dat$Contributiontopotpence~(1|dat$GroupIDforGLMM))
summary(icc2)
#############6% of variation at level of group

#############Models
#############Does an interaction between condition and sex help the model?
#############Model with interaction
fullmod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Condition*dat$Sex+
              dat$Age+dat$Priorknowledge+dat$PropF+(1|dat$Year))
summary(fullmod)
#############Model without interaction
h1mod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$PropF+(1|dat$Year))
summary(h1mod)
#############Comparison
AIC(fullmod)
AIC(h1mod)
#############LR test
library(lmtest)
lrtest(fullmod,h1mod)
#############Interaction improves model fit.

#############Does sex ratio help the model?
#############Model with sex ratio
fullmod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Condition*dat$Sex+
                dat$Age+dat$Priorknowledge+dat$PropF+(1|dat$Year))
summary(fullmod)
#############Model without sex ratio
h2mod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Condition*dat$Sex+
              dat$Age+dat$Priorknowledge+(1|dat$Year))
summary(h2mod)
#############Comparison
AIC(fullmod)
AIC(h2mod)
#############LR test
library(lmtest)
lrtest(fullmod,h2mod)
#############Sex ratio improves model fit.
