##############Load in
setwd("~/desktop/SL Practical")
library(readxl)
dat<-read_excel("AssortmentData_2013-2017.xls",sheet=3)

##############Factorise categorical variables
dat$Condition<-as.factor(dat$Condition)
dat$Sex<-as.factor(dat$Sex)
dat$Year<-as.factor(dat$Year)
dat$PropF<-as.factor(dat$PropF)
dat$Priorknowledge<-as.factor(dat$Priorknowledge)
dat$GroupIDforGLMM<-as.factor(dat$GroupIDforGLMM)
levels(dat$Sex)<-c("Male","Female")
levels(dat$Condition)<-c("Random","Assortment")

##############Since we have differing proportions of females every time, could use below for simplicity?
#dat$RatioMajMin<-ifelse(dat$PropF<=0.49,"min",ifelse(dat$PropF>=0.51,"maj","split"))
#dat$RatioMajMin<-as.factor(dat$RatioMajMin)
##############Or since so few mins, maybe:
#dat$RatioMajMin<-ifelse(dat$PropF>=0.51,"maj","min")
#dat$RatioMajMin<-as.factor(dat$RatioMajMin)

##############Plot prep
library(ggplot2)
dodge<-position_dodge(width=0.75)

##############Violin plot of condition collapsed across years and sexes (use facet wrap to see year/sex)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence))+
  geom_violin()+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.5)+
  #facet_wrap(~Year)+
  theme_classic()

##############Violin plot of sex collapsed across years and conditions (use facet wrap to see year/sex)
ggplot(dat,aes(x=Sex,y=Contributiontopotpence))+
  geom_violin()+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.5)+
  #facet_wrap(~Year)+
  theme_classic()

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
av<-aov(dat$Contributiontopotpence~dat$PropF)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$Sex)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$PropF)
summary(av)
TukeyHSD(av)

#############GLMMs
library(lme4)

#############Before running models, how much variation is attributable to years?
icc1<-lmer(dat$Contributiontopotpence~(1|dat$Year))
summary(icc1)
as.data.frame(VarCorr(icc1),comp="Variance")[1,4]/
  (as.data.frame(VarCorr(icc1),comp="Variance")[1,4]+as.data.frame(VarCorr(icc1),comp="Variance")[2,4])
#############4% of variation at level of year

############Just out of interest, how much variation between groups?
icc2<-lmer(dat$Contributiontopotpence~(1|dat$GroupIDforGLMM))
summary(icc2)
as.data.frame(VarCorr(icc2),comp="Variance")[1,4]/
  (as.data.frame(VarCorr(icc2),comp="Variance")[1,4]+as.data.frame(VarCorr(icc1),comp="Variance")[2,4])
#############6% of variation at level of group

#############Models
#############Does the inclusion of sex ratio help the model?
#############'Null' model containing all main effects but PropF.
emptymod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+(1|dat$Year))

#############Model with sex ratio
sexratiomod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$PropF+(1|dat$Year))

#############Comparison
AIC(emptymod)
AIC(sexratiomod)
library(lmtest)
lrtest(emptymod,sexratiomod)
#############Sex ratio improves model fit.

#############Does an interaction between condition and sex help the model?
#############sexratiomod is the  'null' model containing all main effects but no cond*sex
#############Model with cond*sex added
condsexmod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$PropF+
                   dat$Condition*dat$Sex+(1|dat$Year))

#############Comparison
AIC(sexratiomod)
AIC(condsexmod)
lrtest(sexratiomod,condsexmod)
#############Interaction improves fit.

#############Does sex ratio*condition improve model fit?
#############sexratiomod is the  'null' model containing all main effects but no cond*sex
#############Model with cond*sexratio added
condratiomod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$PropF+
                   dat$Condition*dat$PropF+(1|dat$Year))
#############Won't converge - too few data points. See below:
library(dplyr)
View(dat%>%group_by(Condition,Sex,PropF,Year)%>%tally())

#############We can stop here, or redo everything with a different propF variable.












##############ANOVAs
av<-aov(dat$Contributiontopotpence~dat$RatioMajMin)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$Sex)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$RatioMajMin)
summary(av)
TukeyHSD(av)

#############GLMMs

#############Does the inclusion of sex ratio help the model?
#############'Null' model containing all main effects but PropF.
emptymod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+(1|dat$Year))

#############Model with sex ratio
sexratiomod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$RatioMajMin+(1|dat$Year))

#############Comparison
AIC(emptymod)
AIC(sexratiomod)
library(lmtest)
lrtest(emptymod,sexratiomod)
#############Sex ratio improves model fit.

#############Does an interaction between condition and sex help the model?
#############sexratiomod is the  'null' model containing all main effects but no cond*sex
#############Model with cond*sex added
condsexmod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$RatioMajMin+
                   dat$Condition*dat$Sex+(1|dat$Year))

#############Comparison
AIC(sexratiomod)
AIC(condsexmod)
lrtest(sexratiomod,condsexmod)
#############Interaction improves fit.

#############Does sex ratio*condition improve model fit?
#############sexratiomod is the  'null' model containing all main effects but no cond*sex
#############Model with cond*sexratio added
condratiomod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$RatioMajMin+
                     dat$Condition*dat$RatioMajMin+(1|dat$Year))

#############Comparison
AIC(sexratiomod)
AIC(condratiomod)
lrtest(sexratiomod,condratiomod)
#############Interaction improves fit.


condratiomod<-lmer(dat$Contributiontopotpence~dat$Condition+dat$Sex+dat$Age+dat$Priorknowledge+dat$RatioMajMin+
                     dat$Condition*dat$RatioMajMin*dat$Sex+(1|dat$Year))


#females that assort into majority female groups give the most?
ggplot(dat)+
  aes(x=Sex,y=Contributiontopotpence,fill=RatioMajMin)+
  geom_violin()+
  geom_point(position=position_jitterdodge())+
  facet_wrap(~Condition)

