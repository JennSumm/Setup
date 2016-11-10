##Parasite Data 
setwd("C:/Users/chris/Desktop/RFiles/ParasiteData")

Parasite <- read.table("ParasiteData3.csv", header=T, sep=',')
attach(Parasite)
names (Parasite)

fOrth_Plot<-as.factor(Orth_Plot)
Curve_Orth_Plot

fCurve_Orth_Plot<-as.factor(Curve_Orth_Plot)

fType<-as.factor(Type)
fPASTURE<-as.factor(PASTURE)
fPlot<-as.factor(Plot)
fPat<-as.factor(Pat)

fType<-as.factor(Type_ID)
fPASTURE<-as.factor(PASTURE)
fPlot<-as.factor(Plot_ID)
fPat<-as.factor(Pat_ID)
fPLOT_ID<-as.factor(ID)
library(glmmADMB)
library(MASS)

##This is the full model for a split split plot design 
#http://stats.stackexchange.com/questions/19818/split-split-plot-design-and-lme
#http://www3.imperial.ac.uk/portal/pls/portallive/docs/1/1171923.PDF

mod1 = glmmadmb(Count~ fPat*fPlot*fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(mod1)

library(ggplot2)
newdfr = expand.grid(X = seq(from = 30, to = 130, by = 1),
                     condition = c("A", "B"),
                     Subject =  c("S1", "S2", "S3", "S4", "S5", "S6"))
Parasite$predict.mod1 = predict(mod1, type = "response", newdata = Parasite)
Now, use ggplot{ggplot2} to plot the model:

ggplot(Parasite, aes(x = X, y = Longer/Total, color = condition)) 
  ylab("Probability") + xlab("X") 
  geom_point() 
  geom_line(data = newdfr, aes(x = X, y = predict.mod1)) 
  facet_wrap(~ Subject, ncol = 3)
  scale_x_continuous(breaks= seq(30, 130, by = 30)))

m1 = glmmadmb(Count~ fPlot*fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m0)

m2 = glmmadmb(Count~ fPat*fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m0)

m3 = glmmadmb(Count~ fPat*fPlot  +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m0)

m4 = glmmadmb(Count~ fPat+fPlot  +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m0)

m5 = glmmadmb(Count~ fPat+fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m5)
m6 = glmmadmb(Count~ fType+fPlot  +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m0)
m7 = glmmadmb(Count~ fType+fPlot+fPat  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m7)


AICctab(m1, m2, m3, m4, m5, m6, m7, m0, weights=TRUE,delta=TRUE,base=TRUE,sort=TRUE, logLik = TRUE)

library(bbmle)

wee= aov(Count~fPat*fPlot*fType+ Error(fPASTURE/fPlot))
summary(wee)


library(car)
qqPlot(wee, main="QQ Plot")

plot(residuals(wee[["Within"]])~fitted(wee[["Within"]]))
qqnorm(residuals(wee[["Within"]]))
qqline(residuals(wee[["Within"]]))

summary(wee)

library(lme4)
require(lmerTest)
lme1=lmer(Count~fPat*fPlot*fType  +(1|fPASTURE/fPlot), data = Parasite)

library(nlme)
lme1=lme(Count~fPat*fPlot/fType, random  = ~1|fPASTURE/fPlot, data = Parasite)
summary(lme1)

plot(resid(lme1) ~ fitted(lme1),main="residual plot")
abline(h=0)
qqnorm(resid(lme1), main="Q-Q plot for residuals")
qqline(residuals(lme1))


summary(lme1)
anova(lme1)
plot(lme1)
summary(lm1 <- glmmadmb(Count~ fPat*fPlot*fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
)
slm1 <- step(lm1)
summary(slm1)
slm1$anova

mp = glmmadmb(Count~ fPat*fPlot*fType  +(1|fPASTURE/fPLOT_ID), family= "poisson", data = Parasite)
summary(mp)
##With poisson significance decreases for everything with all p values near 1

mn = glmmadmb(Count~ fPat*fPlot*fType  +(1|fPASTURE/fPLOT_ID), data = Parasite)
summary(mn)
##Same for normal distribution 

m11 = glmmadmb(Count~ fPat+fPlot+fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m11)


##SUm to Zero (only ant treatment with 3 terms)
fType2<-as.factor(Type)
fPASTURE2<-as.factor(PASTURE)
fPlot2<-as.factor(Plot)
fPat2<-as.factor(Pat)


m02 = glmmadmb(Count~ fPat2*fPlot2*fType2  +(1|fPASTURE2/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m02)

mp2 = glmmadmb(Count~ fPat2*fPlot2*fType2  +(1|fPASTURE2/fPLOT_ID), family= "poisson", data = Parasite)
summary(mp2)
##With poisson significance decreases for everything with all p values near 1

mn2 = glmmadmb(Count~ fPat2*fPlot2*fType2  +(1|fPASTURE2/fPLOT_ID), data = Parasite)
summary(mn2)
##Same for normal distribution 

m112 = glmmadmb(Count~ fPat2+fPlot2+fType2  +(1|fPASTURE2/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m112)


##SUm to Zero ALL
fType2<-as.factor(Type_0)
fPASTURE2<-as.factor(PASTURE)
fPlot2<-as.factor(Plot)
fPat2<-as.factor(Pat_0)
CountN<-as.numeric(Count)

m02 = glmmadmb(CountN~ fPat2*fPlot2*fType2  +(1|fPASTURE2/fPLOT_ID), link = "log", family= "nbinom2", data = Parasite)
summary(m02)

mp2 = glmmadmb(Count~ fPat2*fPlot2*fType2  +(1|fPASTURE2/fPLOT_ID), family= "poisson", data = Parasite)
summary(mp2)
##With poisson significance decreases for everything with all p values near 1

mn2 = glmmadmb(Count~ fPat2*fPlot2*fType2  +(1|fPASTURE2/fPLOT_ID), data = Parasite)
summary(mn2)
##Same for normal distribution 

m112 = glmmadmb(Count~ fPat+fPlot+fType  +(1|fPASTURE), link = "log", family= "nbinom2", data = Parasite)
summary(m112)

par(mfrow=c(3,2))
resm6<-residuals(m112,type="pearson")
Fm6<-fitted(m112,level=0)
plot(fitted(m112),resm6,xlab="fitted values", ylab="norm resids")
plot(fPat,resm6,xlab="Pat",ylab="norm resids")
plot(Parasite$fPlot,resm6,xlab="Ant",ylab="norm resids")
plot(Parasite$fType,resm6,xlab="Type",ylab="norm resids")
hist(resm6)
plot(fitted(m112), residuals(m112)
 
      lines(smooth.spline(fitted(m112), residuals(m112))))

plot(m112)
plot.glmm.admb(m112)

library(nlme) 

##DOESNT WORK BECAUSE NOTHING EVER WORKS IN R> NOTHING> 
PQL <- glmmPQL(CountN ~ fPat+fPlot+fType, ~1 | fPASTURE2/fPLOT_ID, family = gaussian(link = "log"),data = Parasite,  start=5)






m0 = glmmadmb(Count~ fPat*fCurve_Orth_Plot*fType  +(1|fPASTURE/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m0)


m0 = glmmadmb(Count~ fPat*fPlot*fType  +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m0)





m1 = lme(Count~ fPat * fPlot *  fType, random = ~ 1|fPASTURE2/fPlot, data = Parasite,weights = varIdent(form =~ 1 | fPASTURE2), method = "REML")
summary(m1)

family= "nbinom2"
m1 = glmmadmb(Count~ fType*fPlot*fPat  +(1|fPASTURE/fPlot),family= "nbinom2", data = Parasite)

, zeroInflation=TRUE


m1 = glmmadmb(Count~ fType+ fPlot*fPat  +(1|fPASTURE/fPlot) , family= "nbinom2", data = Parasite)
summary(m1)

m1 = glmmadmb(Count~ fType*fPlot* fType*fPat  +(1|fPASTURE/fPlot) ,  data = Parasite)
summary(m1)

m1 = glmmadmb(Count~ fPlot* fType*fPat  +(1|fPASTURE/fPlot) ,  data = Parasite)
summary(m1)


m1 = glmmadmb(Count~ fPlot*fPat+fType   +(fPASTURE|fPlot) , data = Parasite)
summary(m1)

m1 = glmmadmb(Count~ fPat  +fPlot+fType+ (1|fPASTURE/fPlot) , family= "nbinom2", data = Parasite)
summary(m1)

m1 = glmmadmb(Count~ fType*fPlot+ fPat  +(fPASTURE|fPlot) , data = Parasite)
summary(m1)







m1 = glmmadmb(Count~ fPlot + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m1)
m2 = glmmadmb(Count~ fPat + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m2)
m3 = glmmadmb(Count~ fType + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m3)

##POTENTIAL INTERACTION TERMS
m4 = glmmadmb(Count~  fPat/fPlot + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m4)
##Nothing SIG

m5 = glmmadmb(Count~  fPlot/fPat + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m5)
##Sham and Unfenced are SIG

m6 = glmmadmb(Count~  fPat/fType + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m6)
#Unfenced almost sig, unfenced by SN also almost sig 

m7 = glmmadmb(Count~  fPlot/fType + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m7)
#Control Plot by SN pasture sig

m8 = glmmadmb(Count~  fType/fPat + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m8)
#Unfenced plots in intentive apsture almost sig
##the impact of pasture type on parasite numbers is different for diff values of pat

#ALL SINGLE TERMS
m1 = glmmadmb(Count~ fPat + fType + fPlot + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m1)

m1 = glmmadmb(Count~ fPat + fType + fPlot + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m1)

##BEST MODELS 

m8 = glmmadmb(Count~ fPat + fPlot + fType + (1|fPlot) + (1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m5)

m7 = glmmadmb(Count~ fPat + fPlot + fType + fPlot/fPat + fPlot/fType + fPat/fPlot + fPat/fType +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m7)

m1 = glmmadmb(Count~ fPat + fPlot + fType + fPlot/fType  +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m1)
m2 = glmmadmb(Count~ fPat + fPlot + fType + fPat/fPlot +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m2)
m3 = glmmadmb(Count~ fPat + fPlot + fType + fPat/fType +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m3)
m4 = glmmadmb(Count~ fPat + fPlot + fType + fPlot/fPat + fPat/fType +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m4)
m5 = glmmadmb(Count~ fPat + fPlot + fType + fPat/fPlot + fPat/fType +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m5)
m6 = glmmadmb(Count~ fPat + fPlot + fType + fPlot/fType + fPat/fType +(1|fPASTURE), family= "nbinom2", data = Parasite)
summary(m6)

AICctab(m1, m2, m3, m4, m5, m6, m7, m8, weights=TRUE,delta=TRUE,base=TRUE,sort=TRUE, logLik = TRUE)
library(bbmle)


##THis was found to be the top model 
m1 = glmmadmb(Count~ fPat + fPlot + fType + fPlot/fType  +(1|fPASTURE2)+ (1|fPlot), family= "nbinom2", data = Parasite)
summary(m1)



m1 = glmmadmb(CountN~ fPat + fPlot + fType   +(1|fPASTURE2/fPLOT_ID), family= "nbinom2", data = Parasite)
summary(m1) 

m1 = glmmadmb(Count~ fPat + fPlot + fType + fPlot/fType  +(1|fPASTURE) + (1|fType), family= "nbinom2", data = Parasite)
summary(m1)





##Use box plots to check for outliers 
TypeBox <- boxplot(Count~fPat, varwidth = TRUE, xlab="Pat Treatment", main = "Boxplot of Parasite Abundance", ylab = "Number of Parasites", data=Parasite)

##identifies outliers
TypeBox$out

dotchart(fPat,labels=row.names(TOTALWorkers), main="Soil Moisture")



##Use histograms to check distribution of data to determine type of model to use 

hist(Count)

##Use shapiro test to test for normality 
shapiro.test(Parasite$Count)

##Subset to groups to test for 


##Checking for collinearity 
names(Parasite)
pairs(Parasite[,1:6])

##Interaction plots for 2 way interactions 

interaction.plot(fPlot, fType, Count, fixed = TRUE)

qqplot(m1)
qqnorm(resid(m0))

ED <- resid(m1, type = "pearson")
mu <- predict (m1, type = "response")
plot(x=mu, y = ED, main = "Pearson Residuals")
E<- (Parasite$Count - mu)
EP2 <- (E/ sqrt(7.630148* mu))
plot(x=mu, y = EP2, main = "Pearson Residuals Scaled")


##Vegetation Height 

setwd("C:/Users/chris/Desktop/RFiles")

ANTS <- read.table("aaWinter.csv", header=T, sep=',')

attach(ANTS)
names (ANTS)

library(glmmADMB)
library(bbmle)
m11= aov(Veg~Type,data=ANTS)
summary(m11)


##What distribution are your data 
#http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html


require(car)
require(MASS)
#Normal
qqp(Count, "norm")
#Lognormal
qqp(Count, "lnorm")
#Negative binomial
nbinom <- fitdistr(Count, "Negative Binomial")
qqp(Count, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#Poisson
poisson <- fitdistr(Count, "Poisson")
qqp(Count, "pois", poisson$estimate)
#Gamma
gamma <- fitdistr(Count, "gamma")
qqp(Count, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
###The y axis represents the observations and the x axis represents the quantiles 
###modeled by the distribution. The solid red line represents a perfect distribution fit 
###and the dashed red lines are the confidence intervals of the perfect distribution fit. 
###You want to pick the distribution for which the largest number of observations falls 
###between the dashed lines. 

