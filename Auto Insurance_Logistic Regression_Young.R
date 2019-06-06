#Brent Young
#PREDICT 411
#Auto Insurance Logistic Regression Project

#Part 0: Load & Prepare Data

library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(rJava)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(mice)
library(Hmisc)
library(xlsxjars)
library(xlsx)
library(VIM)
library(pROC)

# Data Import and Variable Type Changes
setwd("~/R/Insurance")
mydata <- read.csv("logit_insurance.csv")
#Training Data
mydata$INDEX <- as.numeric(mydata$INDEX)
mydata$TARGET_FLAG <- as.factor(mydata$TARGET_FLAG)
mydata$SEX <- as.factor(mydata$SEX)
mydata$EDUCATION <- as.factor(mydata$EDUCATION)
mydata$PARENT1 <- as.factor(mydata$PARENT1)
mydata$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mydata$INCOME)))

mydata$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mydata$HOME_VAL)))
mydata$MSTATUS <- as.factor(mydata$MSTATUS)
mydata$REVOKED <- as.factor(mydata$REVOKED)
mydata$RED_CAR <- as.factor(ifelse(mydata$RED_CAR=="yes", 1, 0))
mydata$URBANICITY <- ifelse(mydata$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
mydata$URBANICITY <- as.factor(mydata$URBANICITY)
mydata$JOB <- as.factor(mydata$JOB)
mydata$CAR_USE <- as.factor(mydata$CAR_USE)
mydata$CAR_TYPE <- as.factor(mydata$CAR_TYPE)
mydata$DO_KIDS_DRIVE <- as.factor(ifelse(mydata$KIDSDRIV > 0, 1, 0 ))
mydata$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mydata$OLDCLAIM)))
mydata$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mydata$BLUEBOOK)))
summary(mydata)

#Part 1: Data Exploration 
#Mydata Quality Check
str(mydata)
summary(mydata)

library(Hmisc)
describe(mydata)

# EDA for Numeric Variables

mydata0<- subset(mydata, TARGET_FLAG == 1 )

par(mfrow=c(3,3))
hist(log(mydata0$TARGET_AMT), col = "#A71930", xlab = "Log TARGET_AMT", main = "Log TARGET_AMT Hist")
hist(mydata$KIDSDRIV, col = "#09ADAD", xlab = "KIDSDRIV", main = "Histogram of KIDSDRIV")
hist(mydata$AGE, col = "#DBCEAC", xlab = "AGE", main = "Histogram of AGE")
boxplot(log(mydata0$TARGET_AMT), col = "#A71930", main = "LOG TARGET_AMT Boxplot")
boxplot(mydata$KIDSDRIV, col = "#09ADAD", main = "Boxplot of KIDSDRIV ")
boxplot(mydata$AGE, col = "#DBCEAC", main = "Boxplot of AGE")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(mydata$HOMEKIDS, col = "#A71930", xlab = "HOMEKIDS", main = "Histogram of HOMEKIDS")
hist(mydata$YOJ, col = "#09ADAD", xlab = "YOJ ", main = "Histogram of YOJ")
hist(mydata$INCOME, col = "#DBCEAC", xlab = "INCOME", main = "Histogram of INCOME")
boxplot(mydata$HOMEKIDS, col = "#A71930", main = "Boxplot of HOMEKIDS")
boxplot(mydata$YOJ, col = "#09ADAD", main = "Boxplot of YOJ")
boxplot(mydata$INCOME, col = "#DBCEAC", main = "Boxplot of INCOME ")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(mydata$HOME_VAL, col = "#A71930", xlab = "HOME_VAL", main = "Histogram of HOME_VAL")
hist(mydata$TRAVTIME, col = "#09ADAD", xlab = "TRAVTIME", main = "Histogram of TRAVTIME")
hist(mydata$BLUEBOOK, col = "#DBCEAC", xlab = "BLUEBOOK", main = "Histogram of BLUEBOOK")
boxplot(mydata$HOME_VAL, col = "#A71930", main = "Boxplot of HOME_VAL")
boxplot(mydata$TRAVTIME, col = "#09ADAD", main = "Boxplot of TRAVTIME")
boxplot(mydata$ BLUEBOOK, col = "#DBCEAC", main = "Boxplot of BLUEBOOK")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(mydata$TIF, col = "#A71930", xlab = "TIF", main = "Histogram of TIF")
hist(mydata$OLDCLAIM, col = "#09ADAD", xlab = "OLDCLAIM", main = "Histogram of OLDCLAIM ")
hist(mydata$CLM_FREQ, col = "#DBCEAC", xlab = "CLM_FREQ", main = "Histogram of CLM_FREQ")
boxplot(mydata$TIF, col = "#A71930", main = "Boxplot of TIF")
boxplot(mydata$OLDCLAIM, col = "#09ADAD", main = "Boxplot of OLDCLAIM")
boxplot(mydata$CLM_FREQ, col = "#DBCEAC", main = "Boxplot of CLM_FREQ")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(mydata$MVR_PTS, col = "#A71930", xlab = "MVR_PTS", main = "Histogram of MVR_PTS")
hist(mydata$CAR_AGE, col = "#09ADAD", xlab = "CAR_AGE", main = "Histogram of CAR_AGE")
boxplot(mydata$MVR_PTS, col = "#A71930", main = "Boxplot of MVR_PTS")
boxplot(mydata$CAR_AGE, col = "#09ADAD", main = "Boxplot of CAR_AGE")
par(mfrow=c(1,1))

# Scatterplot Matrix 

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ mydata$TARGET_AMT + mydata$KIDSDRIV + mydata$AGE + mydata$HOMEKIDS + mydata$YOJ + mydata$INCOME + mydata$HOME_VAL + mydata$TRAVTIME, lower.panel = panel.smooth)
par(mfrow=c(1,1))

pairs(~ mydata$TARGET_AMT + mydata$BLUEBOOK+ mydata$TIF+ mydata$OLDCLAIM + mydata$CLM_FREQ + mydata$MVR_PTS + mydata$CAR_AGE, lower.panel = panel.smooth)
par(mfrow=c(1,1))

#Correlation Matrix
subdatnumcor <- subset(mydata, select=c(
  "KIDSDRIV",
  "AGE",
  "HOMEKIDS",
  "YOJ",
  "INCOME",
  "HOME_VAL",
  "TRAVTIME",
  "BLUEBOOK",
  "TIF",
  "OLDCLAIM",
  "CLM_FREQ",
  "MVR_PTS",
  "CAR_AGE",
  "TARGET_AMT"))

require(corrplot)
mcor <- cor(subdatnumcor)
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)

#EDA for Categorical Variables
library(ggplot2)
#TARGET_FLAG
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(TARGET_FLAG) ) +
  ggtitle("TARGET_FLAG") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# PARENT1
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(PARENT1) ) +
  ggtitle("PARENT1") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#MSTATUS
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(MSTATUS) ) +
  ggtitle("MSTATUS") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# SEX
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(SEX) ) +
  ggtitle("SEX") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

# EDUCATION
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(EDUCATION) ) +
  ggtitle("EDUCATION") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#JOB
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(JOB) ) +
  ggtitle("JOB") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#CAR_USE
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(CAR_USE) ) +
  ggtitle("CAR_USE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#CARTYPE
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(CAR_TYPE) ) +
  ggtitle("CAR_TYPE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#RED_CAR
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(RED_CAR) ) +
  ggtitle("RED_CAR") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#REVOKED
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(REVOKED) ) +
  ggtitle("REVOKED") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))



#URBANICITY
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(URBANICITY) ) +
  ggtitle("URBANICITY") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#DO_KIDS_DRIVE
require(ggplot2)
ggplot(mydata) +
  geom_bar( aes(DO_KIDS_DRIVE) ) +
  ggtitle("DO_KIDS_DRIVE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

########### Part 2: Data Transformation ##################
#Part 2: Data Preparation
library(mice)

#Check for missing values
sapply(mydata, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(mydata,2,pMiss)

library(VIM)
aggr_plot <- aggr(mydata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Split datasets into numerical and categorical

#Numeric
subdatnum <- subset(mydata, select=c(
  "INDEX",
  "KIDSDRIV",
  "AGE",
  "HOMEKIDS",
  "YOJ",
  "INCOME",
  "HOME_VAL",
  "TRAVTIME",
  "BLUEBOOK",
  "TIF",
  "OLDCLAIM",
  "CLM_FREQ",
  "MVR_PTS",
  "CAR_AGE",
  "TARGET_AMT"))

subdatnum.df <- data.frame(subdatnum)

#Categorical
subdatcat <- subset(mydata, select=c(
  "INDEX",
  "TARGET_FLAG",
  "PARENT1",
  "MSTATUS",
  "SEX",
  "EDUCATION",
  "JOB",
  "CAR_USE",
  "CAR_TYPE",
  "RED_CAR",
  "REVOKED",
  "URBANICITY",
  "DO_KIDS_DRIVE"))

subdatcat.df <- data.frame(subdatcat)

# Fix NA's for Training Data
#Run imputation
tempData <- mice(subdatnum.df,m=5,maxit=50,meth='pmm',seed=500)

summary(tempData)

# Inspecting the distribution of original and imputed data for the variables that contained N/A
xyplot(tempData,TARGET_AMT~ CAR_AGE + HOME_VAL + YOJ +  INCOME + AGE,pch=18,cex=1)

densityplot(tempData)

#Check N/A values have been removed
subdatnumimp <- complete(tempData,1)
apply(subdatnumimp,2,pMiss)
summary(subdatnumimp)
sapply(subdatnumimp, function(x) sum(is.na(x)))


#Merge Numeric and Categorical datasets back
data <- merge(subdatnumimp, subdatcat.df, by=c("INDEX"))

#Check data
str (data)
summary (data)

#Trim Data
data$CAR_AGE[data$CAR_AGE < 0 ] <- 0 
data$YOJ [(data$YOJ >= 20)] = 20
data$INCOME [(data$INCOME >= 300000)] = 300000
data$HOME_VAL  [(data$HOME_VAL  >= 650000)] = 650000
data$TRAVTIME [(data$TRAVTIME >= 100)] = 100
data$BLUEBOOK [(data$BLUEBOOK >= 55000)] = 55000
data$TIF [(data$TIF >= 17)] = 17
data$MVR_PTS [(data$MVR_PTS >=8)] = 8

#Create Flag Variables
data$HAVE_HOME_KIDS <- as.factor(ifelse(data$HOMEKIDS > 0, 1, 0 ))
data$EMPLOYED <- as.factor(ifelse(data$YOJ > 0, 1, 0 ))
data$HOME_OWNER <- as.factor(ifelse(data$HOME_VAL> 0, 1, 0 ))
data$SUBMITTED_CLAIM <- as.factor(ifelse(data$CLM_FREQ  > 0, 1, 0 ))
data$HAVE_MVR_PTS<- as.factor(ifelse(data$MVR_PTS> 0, 1, 0 ))

#Create SQRT Transformations of Some of the Variables
data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)
data$SQRT_TIF <- sqrt(data$TIF)
data$LOG_TRAVTIME <- log(data$TRAVTIME)
data$LOG_BLUEBOOK <- log(data$BLUEBOOK)
data$LOG_TIF <- log(data$TIF)
data$LOG_MVR_PTS <- log(data$MVR_PTS)
data$LOG_OLDCLAIM <- log(data$OLDCLAIM)

# Bins for Training Data

#Income
data$INCOME_bin[is.na(data$INCOME)] <- "NA"
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 30000] <- "Low"
data$INCOME_bin[data$INCOME >= 30000 & data$INCOME < 80000] <- "Medium"
data$INCOME_bin[data$INCOME >= 80000] <- "High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

#HOME_VAL
data$HOME_VAL_bin[is.na(data$HOME_VAL)] <- "NA"
data$HOME_VAL_bin[data$HOME_VAL == 0] <- "Zero"
data$HOME_VAL_bin[data$HOME_VAL >= 1 & data$HOME_VAL < 125000] <- "Low"
data$HOME_VAL_bin[data$HOME_VAL >= 125000 & data$HOME_VAL < 300000] <- "Medium"
data$HOME_VAL_bin[data$HOME_VAL >= 300000] <- "High"
data$HOME_VAL_bin <- factor(data$HOME_VAL_bin)
data$HOME_VAL_bin <- factor(data$HOME_VAL_bin, levels=c("NA","Zero","Low","Medium","High"))

#OLDCLAIM
data$OLDCLAIM_bin[is.na(data$OLDCLAIM)] <- "NA"
data$OLDCLAIM_bin[data$OLDCLAIM == 0] <- "Zero"
data$OLDCLAIM_bin[data$OLDCLAIM >= 1 & data$OLDCLAIM < 1000] <- "Low"
data$OLDCLAIM_bin[data$OLDCLAIM >= 1000 & data$OLDCLAIM < 4500] <- "Medium"
data$OLDCLAIM_bin[data$OLDCLAIM >= 4500] <- "High"
data$OLDCLAIM_bin <- factor(data$OLDCLAIM_bin)
data$OLDCLAIM_bin <- factor(data$OLDCLAIM_bin, levels=c("NA","Zero","Low","Medium","High"))

summary(data)

#Correlation Matrix
subdatnum2 <- subset(data, select = c(TARGET_AMT, KIDSDRIV, AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF, OLDCLAIM, CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
par(mfrow=c(1,1))  
require(corrplot)
mcor <- cor(subdatnum2)
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)
par(mfrow=c(1,1))  

############# Part 3: Model Development ######################
# Full Model

Model1 = glm(TARGET_FLAG ~ KIDSDRIV  + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL + MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + TIF + CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + URBANICITY + DO_KIDS_DRIVE + HAVE_HOME_KIDS + EMPLOYED + HOME_OWNER  + SUBMITTED_CLAIM + HAVE_MVR_PTS + INCOME_bin + HOME_VAL_bin + OLDCLAIM_bin, data=data, family = binomial(link="logit"))

summary(Model1)
library(car)
Anova(Model1, type="II", test="Wald")
library(rcompanion)
nagelkerke(Model1)
data$Model1Prediction <- predict(Model1, type = "response")

# Model 2 - Stepwise

model.lower = glm(TARGET_FLAG ~ 1, data=data, family = binomial(link="logit"))

Model1 = glm(TARGET_FLAG ~ KIDSDRIV  + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL + MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + TIF + CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + URBANICITY + DO_KIDS_DRIVE + HAVE_HOME_KIDS + EMPLOYED + HOME_OWNER  + SUBMITTED_CLAIM + HAVE_MVR_PTS + INCOME_bin + HOME_VAL_bin + OLDCLAIM_bin, data=data, family = binomial(link="logit"))

step(model.lower, scope = list(upper=Model1), direction="both", test="Chisq", data=data)

Model2 = glm(formula = TARGET_FLAG ~ URBANICITY + JOB + MVR_PTS + MSTATUS + 
               CAR_TYPE + REVOKED + DO_KIDS_DRIVE + INCOME_bin + CAR_USE + 
               TRAVTIME + TIF + OLDCLAIM_bin + BLUEBOOK + OLDCLAIM + HAVE_HOME_KIDS + 
               EDUCATION + HOME_OWNER + PARENT1 + KIDSDRIV, family = binomial(link = "logit"), data = data)
summary(Model2)
library(car)
Anova(Model2, type="II", test="Wald")
library(rcompanion)
nagelkerke(Model2)
data$Model2Prediction <- predict(Model2, type = "response")

# Model 3- Reduced Model with Transformations

Model3 = glm(formula = TARGET_FLAG ~ URBANICITY + JOB + MVR_PTS + MSTATUS + 
               CAR_TYPE + REVOKED + INCOME_bin + CAR_USE + 
               SQRT_TRAVTIME + SQRT_TIF + OLDCLAIM_bin + LOG_BLUEBOOK + OLDCLAIM + HAVE_HOME_KIDS + 
               EDUCATION + HOME_OWNER + KIDSDRIV, family = binomial(link = "logit"), data = data)

summary(Model3)
library(car)
Anova(Model3, type="II", test="Wald")
library(rcompanion)
nagelkerke(Model3)
data$Model3Prediction <- predict(Model3, type = "response")

#Probit Link Model
Model4 = glm(formula = TARGET_FLAG ~ URBANICITY + JOB + MVR_PTS + MSTATUS + 
               CAR_TYPE + REVOKED + INCOME_bin + CAR_USE + 
               SQRT_TRAVTIME + SQRT_TIF + OLDCLAIM_bin + LOG_BLUEBOOK + OLDCLAIM + HAVE_HOME_KIDS + 
               EDUCATION + HOME_OWNER + KIDSDRIV, family = binomial(link = "probit"), data = data)

summary(Model4)
library(car)
Anova(Model4, type="II", test="Wald")
library(rcompanion)
nagelkerke(Model4)
data$Model4Prediction <- predict(Model4, type = "response")

#Part 4: Performance
AIC(Model1)
AIC(Model2)
AIC(Model3)
AIC(Model4)

BIC(Model1)
BIC(Model2)
BIC(Model3)
BIC(Model4)

print(-2*logLik(Model1, REML = TRUE))
print(-2*logLik(Model2, REML = TRUE))
print(-2*logLik(Model3, REML = TRUE))
print(-2*logLik(Model4, REML = TRUE))

ks_stat(actuals= data$TARGET_FLAG, predictedScores=data$Model1Prediction)
ks_stat(actuals= data$TARGET_FLAG, predictedScores=data$Model2Prediction)
ks_stat(actuals= data$TARGET_FLAG, predictedScores=data$Model3Prediction)
ks_stat(actuals= data$TARGET_FLAG, predictedScores=data$Model4Prediction)

library(Deducer)
rocplot(Model1)
rocplot(Model2)
rocplot(Model3)
rocplot(Model4)

coef(Model3)

#Part 5: Stand Alone Scoring Program
setwd("~/R/Data")
mytest <- read.csv("logit_insurance_test.csv")

#Test Data
mytest$INDEX <- as.numeric(mytest$INDEX)
mytest$TARGET_FLAG <- as.factor(mytest$TARGET_FLAG)
mytest$SEX <- as.factor(mytest$SEX)
mytest$EDUCATION <- as.factor(mytest$EDUCATION)
mytest$PARENT1 <- as.factor(mytest$PARENT1)
mytest$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mytest$INCOME)))

mytest$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mytest$HOME_VAL)))
mytest$MSTATUS <- as.factor(mytest$MSTATUS)
mytest$REVOKED <- as.factor(mytest$REVOKED)
mytest$RED_CAR <- as.factor(ifelse(mytest$RED_CAR=="yes", 1, 0))
mytest$URBANICITY <- ifelse(mytest$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
mytest$URBANICITY <- as.factor(mytest$URBANICITY)
mytest$JOB <- as.factor(mytest$JOB)
mytest$CAR_USE <- as.factor(mytest$CAR_USE)
mytest$CAR_TYPE <- as.factor(mytest$CAR_TYPE)
mytest$DO_KIDS_DRIVE <- as.factor(ifelse(mytest$KIDSDRIV > 0, 1, 0 ))
mytest$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mytest$OLDCLAIM)))
mytest$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", mytest$BLUEBOOK)))
summary(mytest)

# Fix NA's for Test 
library(mice)

#Check for missing values
sapply(mytest, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(mytest,2,pMiss)

library(VIM)
aggr_plot <- aggr(mytest, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mytest), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Split datasets into numerical and categorical

#Numeric
subdatnumtest <- subset(mytest, select=c(
  "INDEX",
  "KIDSDRIV",
  "AGE",
  "HOMEKIDS",
  "YOJ",
  "INCOME",
  "HOME_VAL",
  "TRAVTIME",
  "BLUEBOOK",
  "TIF",
  "OLDCLAIM",
  "CLM_FREQ",
  "MVR_PTS",
  "CAR_AGE",
  "TARGET_AMT"))

subdatnumtest.df <- data.frame(subdatnumtest)

#Categorical
subdatcattest <- subset(mytest, select=c(
  "INDEX",
  "TARGET_FLAG",
  "PARENT1",
  "MSTATUS",
  "SEX",
  "EDUCATION",
  "JOB",
  "CAR_USE",
  "CAR_TYPE",
  "RED_CAR",
  "REVOKED",
  "URBANICITY",
  "DO_KIDS_DRIVE"))

subdatcattest.df <- data.frame(subdatcattest)

# Fix NA's for Test Data
#Run imputation
tempDatatest <- mice(subdatnumtest.df,m=5,maxit=50,meth='pmm',seed=500)

summary(tempDatatest)

# Inspecting the distribution of original and imputed data for the variables that contained N/A
xyplot(tempDatatest,TARGET_AMT~ CAR_AGE + HOME_VAL + YOJ +  INCOME + AGE,pch=18,cex=1)

densityplot(tempDatatest)

#Check N/A values have been removed
subdatnumimptest <- complete(tempDatatest,1)
apply(subdatnumimptest,2,pMiss)
summary(subdatnumimptest)
sapply(subdatnumimptest, function(x) sum(is.na(x)))

#Merge Numeric and Categorical datasets back
test <- merge(subdatnumimptest, subdatcattest.df, by=c("INDEX"))

#Check data
str (test)
summary (test)

#Trim Test
test$CAR_AGE[test$CAR_AGE < 0 ] <- 0 
test$YOJ [(test$YOJ >= 20)] = 20
test$INCOME [(test$INCOME >= 300000)] = 300000
test$HOME_VAL  [(test$HOME_VAL  >= 650000)] = 650000
test$TRAVTIME [(test$TRAVTIME >= 100)] = 100
test$BLUEBOOK [(test$BLUEBOOK >= 55000)] = 55000
test$TIF [(test$TIF >= 17)] = 17
test$MVR_PTS [(test$MVR_PTS >=8)] = 8

#Create Flag Variables
test$HAVE_HOME_KIDS <- as.factor(ifelse(test$HOMEKIDS > 0, 1, 0 ))
test$EMPLOYED <- as.factor(ifelse(test$YOJ > 0, 1, 0 ))
test$HOME_OWNER <- as.factor(ifelse(test$HOME_VAL> 0, 1, 0 ))
test$SUBMITTED_CLAIM <- as.factor(ifelse(test$CLM_FREQ  > 0, 1, 0 ))
test$HAVE_MVR_PTS<- as.factor(ifelse(test$MVR_PTS> 0, 1, 0 ))

#Create SQRT Transformations of Some of the Variables
test$SQRT_TRAVTIME <- sqrt(test$TRAVTIME)
test$SQRT_BLUEBOOK <- sqrt(test$BLUEBOOK)
test$SQRT_TIF <- sqrt(test$TIF)
test$LOG_TRAVTIME <- log(test$TRAVTIME)
test$LOG_BLUEBOOK <- log(test$BLUEBOOK)
test$LOG_TIF <- log(test$TIF)
test$LOG_MVR_PTS <- log(test$MVR_PTS)
test$LOG_OLDCLAIM <- log(test$OLDCLAIM)

# Bins for Test Data

#Income
test$INCOME_bin[is.na(test$INCOME)] <- "NA"
test$INCOME_bin[test$INCOME == 0] <- "Zero"
test$INCOME_bin[test$INCOME >= 1 & test$INCOME < 30000] <- "Low"
test$INCOME_bin[test$INCOME >= 30000 & test$INCOME < 80000] <- "Medium"
test$INCOME_bin[test$INCOME >= 80000] <- "High"
test$INCOME_bin <- factor(test$INCOME_bin)
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

#HOME_VAL
test$HOME_VAL_bin[is.na(test$HOME_VAL)] <- "NA"
test$HOME_VAL_bin[test$HOME_VAL == 0] <- "Zero"
test$HOME_VAL_bin[test$HOME_VAL >= 1 & test$HOME_VAL < 125000] <- "Low"
test$HOME_VAL_bin[test$HOME_VAL >= 125000 & test$HOME_VAL < 300000] <- "Medium"
test$HOME_VAL_bin[test$HOME_VAL >= 300000] <- "High"
test$HOME_VAL_bin <- factor(test$HOME_VAL_bin)
test$HOME_VAL_bin <- factor(test$HOME_VAL_bin, levels=c("NA","Zero","Low","Medium","High"))

#OLDCLAIM
test$OLDCLAIM_bin[is.na(test$OLDCLAIM)] <- "NA"
test$OLDCLAIM_bin[test$OLDCLAIM == 0] <- "Zero"
test$OLDCLAIM_bin[test$OLDCLAIM >= 1 & test$OLDCLAIM < 1000] <- "Low"
test$OLDCLAIM_bin[test$OLDCLAIM >= 1000 & test$OLDCLAIM < 4500] <- "Medium"
test$OLDCLAIM_bin[test$OLDCLAIM >= 4500] <- "High"
test$OLDCLAIM_bin <- factor(test$OLDCLAIM_bin)
test$OLDCLAIM_bin <- factor(test$OLDCLAIM_bin, levels=c("NA","Zero","Low","Medium","High"))

summary(test)

#Stand Alone Scoring Program
data0<- subset(data, TARGET_FLAG == 1 )

test$P_TARGET_FLAG <- predict(Model3, newdata = test, type = "response")

targetbycar <- aggregate(data0$TARGET_AMT, list(data0$CAR_TYPE), mean)

test$P_TARGET_AMT <- ifelse(test$CAR_TYPE=="Minivan", 5601.665%*%.27,
                            ifelse(test$CAR_TYPE=="Panel Truck", 7464.703%*%.27,
                                   ifelse(test$CAR_TYPE=="Pickup", 5430.106%*%.27,
                                          ifelse(test$CAR_TYPE=="Sports Car", 5412.733%*%.27,
                                                 ifelse(test$CAR_TYPE=="Van", 6908.553%*%0.27, 5241.104%*%.27)))))

# Scored Data File
#subset of data set for the deliverable "Scored data file"
scores <- test[c("INDEX","P_TARGET_FLAG", "P_TARGET_AMT")]

#Note, this next function will output a csv file in your work environment called write.csv.

write.csv(scores, file = "CI_Scored.csv")
write.csv(as.data.frame(scores), file = "logit_insurance_test.csv", 
          sheetName = "Scored Data File", row.names = FALSE)


