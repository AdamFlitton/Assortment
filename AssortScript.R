##############Load
setwd("~/desktop/SL Practical")
library(readxl)
dat<-read_excel("AssortmentData_2013-2017.xls",sheet=3)

##############Factorise categorical variables
dat$Condition<-as.factor(dat$Condition)
dat$Sex<-as.factor(dat$Sex)
dat$Year<-as.factor(dat$Year)
dat$Priorknowledge<-as.factor(dat$Priorknowledge)
levels(dat$Sex)<-c("Male","Female")
levels(dat$Condition)<-c("Random","Assortment")

##############Plot prep
library(ggplot2)
dodge<-position_dodge(width=0.75)

##############Violin plot of condition collapsed across years and sexes (use facet wrap to see year/sex)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence))+
  geom_violin()+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.5)+
  facet_wrap(~Year)+
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
  facet_wrap(~Year)+
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
t.test(dat$PropF~dat$Condition)

##############ANOVAs
av<-aov(dat$Contributiontopotpence~dat$PropF)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$Sex)
av<-aov(dat$Contributiontopotpence~dat$Condition*dat$PropF)
summary(av)
TukeyHSD(av)

#############GLMMs
library(lme4)

#############ICC for year
icc1<-lmer(dat$Contributiontopotpence~(1|dat$Year))
summary(icc1)
as.data.frame(VarCorr(icc1),comp="Variance")[1,4]/
  (as.data.frame(VarCorr(icc1),comp="Variance")[1,4]+as.data.frame(VarCorr(icc1),comp="Variance")[2,4])
#############4% of variation at level of year

#############Without year as random intercept
fullmodel<-lm(Contributiontopotpence~Condition+Sex+Age+Priorknowledge+Condition*Sex,dat)
summary(fullmodel)
AIC(fullmodel)
#1413.03
model2<-lm(Contributiontopotpence~Condition+Sex+Age+Priorknowledge,dat)
AIC(model2)
#1416.70
model3<-lm(Contributiontopotpence~Sex+Age+Priorknowledge,dat)
AIC(model3)
#1414.72
model4<-lm(Contributiontopotpence~Sex+Age,dat)
AIC(model4)
#1412.79
model5<-lm(Contributiontopotpence~Sex,dat)
AIC(model5)
#1410.82
model6<-lm(Contributiontopotpence~1,dat)
AIC(model6)
#1411.35

#############With year as random intercept
fullmodel<-lmer(Contributiontopotpence~Condition+Sex+Age+Priorknowledge+Condition*Sex+(1|Year),dat)
summary(fullmodel)
AIC(fullmodel)
#1384.19
model2<-lmer(Contributiontopotpence~Condition+Sex+Age+Priorknowledge+(1|Year),dat)
AIC(model2)
#1394.19
model3<-lmer(Contributiontopotpence~Sex+Age+Priorknowledge+(1|Year),dat)
AIC(model3)
#1398.52
model4<-lmer(Contributiontopotpence~Sex+Age+(1|Year),dat)
AIC(model4)
#1402.09
model5<-lmer(Contributiontopotpence~Sex+(1|Year),dat)
AIC(model5)
#1402.05
model6<-lmer(Contributiontopotpence~(1|Year),dat)
AIC(model6)
#1407.44
#Model below is simplest, and lowest AIC.
fullmodelnoage<-lmer(Contributiontopotpence~Condition+Sex+Priorknowledge+Condition*Sex+(1|Year),dat)
AIC(fullmodelnoage)
#1384.19


#############Sex ratio ideas

graphdat<-aggregate(dat$Sex,
                    by=list(cond=dat$Condition),
                    FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
graphdat<-do.call(data.frame,graphdat)
graphdat$se<-graphdat$x.sd/sqrt(graphdat$x.n)
colnames(graphdat)<-c("cond","mean","sd","n","se")
graphdat$cond<-as.factor(graphdat$cond)

ggplot(graphdat,aes(x=cond,y=mean))+
  geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3)+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=0.2,
                position=position_dodge(0.9))+
  ylim(c(0,1))

#Need to read this more - how about residuals? Is it the same as a partial regression/ Would it even help given
#the boundary on the N of males you can have given the bias towards female N?
res<-residuals(lmer(Contributiontopotpence~PropF*Condition+Condition+PropF+Priorknowledge+(1|Year),dat))
restest<-lmer(res~Sex+(1|Year),dat)

summary(restest)

dat$res<-res

ggplot(dat,aes(x=Sex,y=res))+
  geom_violin()+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.5)+
  #facet_wrap(~Year)+
  theme_classic()


#############Testing sex ratio
fullmodel<-lmer(Contributiontopotpence~Condition+PropF+Age+Priorknowledge+Condition*PropF+(1|Year),dat)
summary(fullmodel)
AIC(fullmodel)
#1383.75

model2<-lmer(Contributiontopotpence~Condition+PropF+Age+Priorknowledge+(1|Year),dat)
AIC(model2)
#1394.82

model3<-lmer(Contributiontopotpence~PropF+Age+Priorknowledge+(1|Year),dat)
AIC(model3)
#1398.41

model4<-lmer(Contributiontopotpence~PropF+Age+(1|Year),dat)
AIC(model4)
#1402.09

model5<-lmer(Contributiontopotpence~PropF+(1|Year),dat)
AIC(model5)
#1402.16

model6<-lmer(Contributiontopotpence~(1|Year),dat)
AIC(model6)
#1407.44

#drop age
fullmodelnoage<-lmer(Contributiontopotpence~Condition+PropF+Priorknowledge+Condition*PropF+(1|Year),dat)
AIC(fullmodelnoage)


#Predictors of assortation:
model6<-lm(PropF~1,dat)
model7<-lmer(PropF~(1|Year),dat)
icc(model7)
AIC(model7)
model8<-lmer(PropF~Sex+(1|Year),dat)
summary(model8)
AIC(model8)
