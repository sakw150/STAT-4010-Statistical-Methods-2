library(ggplot2)
Data=read.table('SaYoPillow.csv',header=TRUE, sep=",")
names(Data)
Sleep=data.frame(SnoringRange=Data$sr,RespirationRate=Data$rr,BodyTemperature=Data$t,
                LimbMovementRate=Data$lm,BloodOxygenLevels=Data$bo,EyeMovement=Data$rem,
                HoursofSleep=Data$m,HeartRate=Data$hr,StressLevel=Data$sl)
summary(Sleep)

par(mfrow = c(1,3))
boxplot(Sleep$SnoringRange, main='Snoring Range',col="#CFB87C")
boxplot(Sleep$RespirationRate, main='Respiration Rate',col="#CFB87C")
boxplot(Sleep$BodyTemperature, main='Body Temperature',col="#CFB87C")

par(mfrow = c(1,3))
boxplot(Sleep$LimbMovementRate, main='Limb Movement Rate',col="#CFB87C")
boxplot(Sleep$BloodOxygenLevels, main='Blood Oxygen Levels',col="#CFB87C")
boxplot(Sleep$EyeMovement, main='Eye Movement',col="#CFB87C")

par(mfrow = c(1,3))
boxplot(Sleep$HoursofSleep, main='Hours of Sleep',col="#CFB87C")
boxplot(Sleep$HeartRate, main='Heart Rate',col="#CFB87C")
boxplot(Sleep$StressLevel, main='Stress Level',col="#CFB87C")

# Correlation and Pairs Plot 
library(corrplot)
col4 = colorRampPalette(c("black", "darkgrey", "grey","#CFB87C"))
corrplot(cor(Sleep), method = "ellipse", col = col4(100), 
         addCoef.col = "black", tl.col = "black")
pairs(Sleep, main = "Sleep Data",  pch = 21,bg = c("#CFB87C"))

#GLM Using Poisson 
glmod2_stress=glm(StressLevel~SnoringRange+RespirationRate+BodyTemperature+
                  LimbMovementRate+BloodOxygenLevels+EyeMovement+HoursofSleep,
                  data=Sleep,family=poisson)
summary(glmod2_stress)
par(mfrow=c(2,2))
plot(glmod2_stress)

# Code to Modify Stress Into Binomial 
library(readr)
library(dplyr)
library(tidyr)
SleepB=mutate(Sleep,StressLevel=ifelse(StressLevel>=2,1,0))
summary(SleepB)

# Binomial Regression Attempt 
glmodB_stress=glm(StressLevel~SnoringRange+RespirationRate+BodyTemperature+
                  LimbMovementRate+BloodOxygenLevels+EyeMovement+HoursofSleep,
                  data=SleepB,family=binomial)
summary(glmodB_stress)

# Full Linear Regression Without Any Data Transformation 
lmod_REM2=lm(EyeMovement~SnoringRange+RespirationRate+BodyTemperature+LimbMovementRate+
             BloodOxygenLevels+HoursofSleep,data=Sleep)
summary(lmod_REM2)
par(mfrow=c(2,2))
plot(lmod_REM2)

#Modified Sqrt Transformation of Original Data 
Sleep$EyeMovementTransform=sqrt(Sleep$EyeMovement)
Sleep$SnoringRangeTransform=sqrt(Sleep$SnoringRange)
Sleep$RespirationRateTransform=sqrt(Sleep$RespirationRate)
Sleep$BodyTemperatureTransform=sqrt(Sleep$BodyTemperature)
Sleep$LimbMovementRateTransform=sqrt(Sleep$LimbMovementRate)
Sleep$BloodOxygenLevelsTransform=sqrt(Sleep$BloodOxygenLevels)
Sleep$HoursofSleepTransform=sqrt(Sleep$HoursofSleep)

lmod_REM3=lm(EyeMovementTransform~SnoringRangeTransform+RespirationRateTransform+
             BodyTemperatureTransform+LimbMovementRateTransform+BloodOxygenLevelsTransform+
             HoursofSleepTransform,data=Sleep)
summary(lmod_REM3)

par(mfrow=c(2,2))
plot(lmod_REM2)

#Modified Scale Transformation (Normalization )
Sleep3=as.data.frame(scale(Sleep[1:9]))

#Only Linear Section RespirationRate
library(car)
lmod_Resp=lm(RespirationRate~HeartRate+SnoringRange+LimbMovementRate+EyeMovement,data=Sleep3)
summary(lmod_Resp)
par(mfrow=c(2,2))
plot(lmod_Resp)
durbinWatsonTest(lmod_Resp)

# Adjusted Model AIC 
lmod_RespAdj=lm(RespirationRate~HeartRate+EyeMovement,data=Sleep3)
summary(lmod_RespAdj)
par(mfrow=c(2,2))
plot(lmod_RespAdj)

# Adjusted Model BIC 
lmod_RespAdj2=lm(RespirationRate~HeartRate+LimbMovementRate,data=Sleep3)
summary(lmod_RespAdj2)
par(mfrow=c(2,2))
plot(lmod_RespAdj2)

# AIC 
library(leaps)
library(MASS)
n = dim(Sleep3)[1]; 
reg1 = regsubsets(RespirationRate~HeartRate+SnoringRange+LimbMovementRate+EyeMovement,
  data = Sleep3)
rs = summary(reg1)
rs$which
AIC=2*(2:5)+n*log(rs$rss/n)
plot(AIC~I(1:4),xlab="Number of Predictors", ylab="AIC")

#BIC 
BIC = log(n)*(2:5) + n*log(rs$rss/n) 
#BIC
plot(BIC ~ I(1:4), xlab = "Number of Predictors", ylab = "BIC")
vif(lmod_RespAdj)
kappa(lmod_RespAdj)
