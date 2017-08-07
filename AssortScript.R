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
library(dplyr)
dodge<-position_dodge(width=0.75)

##############Condition collapsed across year and sex.
datforplot1<-dat %>% group_by(Condition) %>% summarise(avg=mean(Contributiontopotpence),std=sd
                                                       (Contributiontopotpence),n=length(Contributiontopotpence))
datforplot1<-data.frame(datforplot1)
datforplot1$se<-datforplot1$std/(sqrt(datforplot1$n))
datforplot1$Condition<-as.factor(datforplot1$Condition)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence))+
  geom_violin(position=dodge)+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.5)+
  geom_point(aes(y=avg),data=datforplot1)+
  geom_errorbar(aes(y=avg,ymin=avg-se,ymax=avg+se),data=datforplot1)+
  ylab("Contribution to Pot (Pence)")+
  theme(panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14),axis.text=element_text(size=12),
        strip.background=element_blank(),strip.text=element_text(size=12),
        legend.text=element_text(size=12))

##############Condition by sex interaction collapsed across years.
datforplot2<-dat %>% group_by(Condition,Sex) %>% summarise(avg=mean(Contributiontopotpence),std=sd
                                                           (Contributiontopotpence),n=length(Contributiontopotpence))
datforplot2<-data.frame(datforplot2)
datforplot2$se<-datforplot2$std/(sqrt(datforplot2$n))
datforplot2$Condition<-as.factor(datforplot2$Condition)
datforplot2$Sex<-as.factor(datforplot2$Sex)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence,fill=Sex))+
  geom_violin(position=dodge)+
  geom_point(position=position_jitterdodge())+
  geom_errorbar(aes(y=avg,ymin=avg-se,ymax=avg+se),position=dodge,data=datforplot2)+
  ylab("Contribution to Pot (Pence)")+
  theme(panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14),axis.text=element_text(size=12),
        strip.background=element_blank(),strip.text=element_text(size=12),
        legend.text=element_text(size=12))

##############Condition effect for each year collapsed across sexes.
datforplot3<-dat %>% group_by(Condition,Year) %>% summarise(avg=mean(Contributiontopotpence),std=sd
                                                            (Contributiontopotpence),n=length(Contributiontopotpence))
datforplot3<-data.frame(datforplot3)
datforplot3$se<-datforplot3$std/(sqrt(datforplot3$n))
datforplot3$Condition<-as.factor(datforplot3$Condition)
datforplot3$Year<-as.factor(datforplot3$Year)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence))+
  geom_violin(position=dodge)+
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.5)+
  geom_point(aes(y=avg),data=datforplot3)+
  geom_errorbar(aes(y=avg,ymin=avg-se,ymax=avg+se),data=datforplot3)+
  facet_wrap(~Year)+
  ylab("Contribution to Pot (Pence)")+
  theme(panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14),axis.text=element_text(size=12),
        strip.background=element_blank(),strip.text=element_text(size=12),
        legend.text=element_text(size=12))


##############interaction between condition and sex for each year.
datforplot4<-dat %>% group_by(Condition,Sex,Year) %>% summarise(avg=mean(Contributiontopotpence),std=sd
                                                                (Contributiontopotpence),n=length(Contributiontopotpence))
datforplot4<-data.frame(datforplot4)
datforplot4$se<-datforplot4$std/(sqrt(datforplot4$n))
datforplot4$Condition<-as.factor(datforplot4$Condition)
datforplot4$Sex<-as.factor(datforplot4$Sex)
ggplot(dat,aes(x=Condition,y=Contributiontopotpence,fill=Sex))+
  geom_violin(position=dodge)+
  geom_point(position=position_jitterdodge())+
  geom_errorbar(aes(y=avg,ymin=avg-se,ymax=avg+se),position=dodge,data=datforplot4)+
  facet_wrap(~Year)+
  theme(panel.border=element_blank(),panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14),axis.text=element_text(size=12),
        strip.background=element_blank(),strip.text=element_text(size=12),
        legend.text=element_text(size=12))

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

icc(fullmodelnoage)
summary(fullmodelnoage)

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


#Predictors of assortment:
model6<-lm(PropF~1,dat)
model7<-lmer(PropF~(1|Year),dat)
icc(model7)
AIC(model7)
model8<-lmer(PropF~Sex+(1|Year),dat)
summary(model8)
AIC(model8)

