cart <- read.table("HR_Employee_Attrition_Data.csv",sep = ",", header = T)

# Data Exploration
summary(cart) # Employee count, Over18 have just one value. This could be removed
head(cart)
str(cart) # DATA TYPE
nrow(cart) #NO OF ROWS
ncol(cart) #NO OF COLUMNS
names (cart) #NAMES OF VARIABLES

library(caret)
nsv <- nearZeroVar(cart, saveMetrics=TRUE)
nsv <-cbind("ColNo"=1:ncol(cart),nsv)
nsv # this shows standard hours has just one value, so remove this as well

# Remove variables which are not required or no variations
cart = subset(cart, select = c(-EmployeeCount, -EmployeeNumber, -Over18,-StandardHours))
any(is.na(cart)) # check for missing values

length(which(cart$Attrition=="Yes"))/nrow(cart) # attrition rate = 16% 
# Since this is good enough percentage we would not do any over-under sampling

attach(cart)
library(ggplot2)
ggplot(cart,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
# People of around age 30-35 have more attrition

table(Attrition, BusinessTravel)
prop.table(table(BusinessTravel,Attrition),1)*100
# High percentage of frequently travellers in attrition

table(Attrition, Department)
prop.table(table(Department,Attrition),1)*100
# Less attrition from research and development

table(Attrition, Education)
prop.table(table(Education,Attrition),1)*100
#slight variation in attrition rates

table(Attrition, EducationField)
prop.table(table(EducationField,Attrition),1)*100
#slight variation in attrition rates

ggplot(cart,aes(HourlyRate,fill=Attrition))+geom_bar()
# Seems to vary less or no specific pattern

ggplot(cart,aes(MonthlyIncome,fill=Attrition))+geom_density()
# More attrition in lower income group

boxplot(PercentSalaryHike~Attrition)
aggregate(PercentSalaryHike,by = list(Attrition), mean)
# Not much of difference in mean, but boxplot shows lower level for hikes in people who moved out

table(Attrition, StockOptionLevel)
prop.table(table(StockOptionLevel,Attrition),1)*100
# High percentage of 0 and 3

table(Attrition, YearsWithCurrManager)
prop.table(table(YearsWithCurrManager,Attrition),1)*100
# Generally leeser the years with current manager, more the attrition rate

ggplot(cart,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()
# High attrition rate in 0 years column

# Convert the below variables into factor type
# cart$Education = as.factor(cart$Education)
# cart$EnvironmentSatisfaction = as.factor(cart$EnvironmentSatisfaction)
# cart$JobInvolvement = as.factor(cart$JobInvolvement)
# cart$JobLevel = as.factor(cart$JobLevel)
# cart$JobSatisfaction = as.factor(cart$JobSatisfaction)
# cart$RelationshipSatisfaction = as.factor(cart$RelationshipSatisfaction)
# cart$StockOptionLevel = as.factor(cart$StockOptionLevel)
# cart$WorkLifeBalance = as.factor(cart$WorkLifeBalance)


# Hypothesis formations
cart$Attrition = as.factor(cart$Attrition)
attach(cart)

# Attrition is not dependent on departments
chisq.test(Attrition,Department)
# significant p value
prop.table(table(Department,Attrition),1)*100
# Reserach has high value, rest two are same

# Distance does not influence attrition
summary(glm(Attrition~DistanceFromHome, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.


# Environment satisfaction is not factor for attrition
summary(glm(Attrition~EnvironmentSatisfaction, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(EnvironmentSatisfaction,Attrition),1)*100
# satisfcation level 1 seems to have high attrition
# Rest have approx same level (try to club if not clubbed in cart)


# No relationship with gender
chisq.test(Attrition,Gender)
# not a significant variable
prop.table(table(Gender,Attrition),1)*100
cart2 = subset(cart, select = -Gender)

# Joblevel does not influence attrition
summary(glm(Attrition~JobLevel, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(JobLevel,Attrition),1)*100
# This seems to have clear demarcation for different levels
# (Cannot be clubbed)

# Joblevel and JobInvolvement not correlated
chisq.test(JobLevel,JobInvolvement)
#significantp-value


# JobRole and JobLevel not related
chisq.test(JobLevel,JobRole)
# highly significant p value, so not related

# Jobinvolvement and job role are not related
chisq.test(JobInvolvement,JobRole)
# High p value hence reject null hypothesis
# we could proceed with just one variable
cart2 = subset(cart2, select = -JobInvolvement)
# Job Satisfcation not related with attrition
summary(glm(Attrition~JobSatisfaction, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(JobSatisfaction,Attrition),1)*100
# Rest have approx same level (try to club if not clubbed in cart)


# Marital status not influence attrition
chisq.test(Attrition,MaritalStatus)
# significant p value
prop.table(table(MaritalStatus,Attrition),1)*100

# Monthly income not a factor for attrition
summary(glm(Attrition~MonthlyIncome, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.


# Monthly rate and Income not related
cor(MonthlyRate, MonthlyIncome)
# very low correlation

# Num companies does not influence attrition
summary(glm(Attrition~NumCompaniesWorked, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(NumCompaniesWorked,Attrition),1)*100
# Rest have approx same level (try to club if not clubbed in cart)


# Overtime does not influence attrition
summary(glm(Attrition~OverTime, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(OverTime,Attrition),1)*100
# Rest have approx same level (try to club if not clubbed in cart)

# Performance Rating does not influence attrition
summary(glm(Attrition~PerformanceRating, data = cart, family = "binomial"))
#Not a significant factor
prop.table(table(PerformanceRating,Attrition),1)*100
# Approximately same value, can be ignored
cart2 = subset(cart2, select = -PerformanceRating)

# Relationship status does not influence attrition
summary(glm(Attrition~RelationshipSatisfaction, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(RelationshipSatisfaction,Attrition),1)*100
# Rest have approx same level (try to club if not clubbed in cart)

# Stock Options Level does not influence attrition
summary(glm(Attrition~StockOptionLevel, data = cart, family = "binomial"))
#Significant p-value hence we reject the null hypothesis.
prop.table(table(StockOptionLevel,Attrition),1)*100
# Rest have approx same level (try to club if not clubbed in cart)

# Pattern of stay in past companies (how stable were they in previous companies)
cart$stability = ifelse(cart$NumCompaniesWorked !=0, cart$TotalWorkingYears/cart$NumCompaniesWorked,0)
ggplot(cart,aes(cart$stability))+geom_density()+facet_grid(~cart$Attrition)
summary(glm(Attrition~cart$stability, data = cart, family = "binomial"))
# Slight difference is observed visually, but glm shows this is important variable

library(dplyr)
medianSal = aggregate(MonthlyIncome~Department+TotalWorkingYears, FUN = median)
temp = merge(cart,medianSal, by.x = c("Department","TotalWorkingYears"), by.y = c("Department","TotalWorkingYears"))

cart$CompareSal = temp$MonthlyIncome.x/temp$MonthlyIncome.y
ggplot(cart,aes(CompareSal))+geom_density()+facet_grid(~Attrition)

# Create buckets for classifications 

cart$AgeGroup <- with(cart,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

cart$DistanceGroup <- with(cart,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups

cart$YearsWithManagerGroup <- with(cart,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups

cart$stability <- with(cart,ifelse(stability>35,9,ifelse(stability>30,8,ifelse(stability>25,7,ifelse(stability>20,6,ifelse(stability>15,5,ifelse(stability>10,4,ifelse(stability>5,3,ifelse(stability>2,2,1))))))))) #Creating Tenure Per Job groups

cart$WorkYearGroup <- with(cart,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

cart$NumCompGroup <- with(cart,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked

cart$DailyRateGroup <- with(cart,ifelse(DailyRate>=1250,6,ifelse(DailyRate>1000,5,ifelse(DailyRate>750,4,ifelse(DailyRate>500,3,ifelse(DailyRate > 250,2,1))))))

cart$HourlyRateGroup <- with(cart,ifelse(HourlyRate>=90,7,ifelse(HourlyRate>80,6,ifelse(HourlyRate>70,5,ifelse(HourlyRate>60,4,ifelse(HourlyRate > 50,3,ifelse(HourlyRate>40,2,1)))))))

cart$MonthlyRateGroup <- with(cart,ifelse(MonthlyRate>=25000,6,ifelse(MonthlyRate>20000,5,ifelse(MonthlyRate>15000,4,ifelse(MonthlyRate>10000,3,ifelse(MonthlyRate > 5000,2,1))))))

cart$MonthlyIncomeGroup <- with(cart,ifelse(MonthlyIncome>=18000,7,ifelse(MonthlyIncome>15000,6,ifelse(MonthlyIncome>12000,5,ifelse(MonthlyIncome>9000,4,ifelse(MonthlyIncome > 6000,3,ifelse(MonthlyIncome>3000,2,1)))))))

cart = subset(cart, select = c(-NumCompaniesWorked,-DistanceFromHome,-Age,-YearsWithCurrManager,-TotalWorkingYears, -DailyRate,-HourlyRate,-MonthlyRate,-MonthlyIncome))

## Creat Development and Validation Sample
set.seed(1234)
cart$random <- runif(nrow(cart), 0, 1);
cart <- cart[order(cart$random),]
#SEPARATE DATA BASED ON VALUE OF RANDOM COLUMN
cart.dev <- cart[which(cart$random <= 0.7),] 
cart.val <- cart[which(cart$random > 0.7),]

#cart2.dev <- cart2[which(cart$random <= 0.7),] 
#cart2.val <- cart2[which(cart$random > 0.7),]

#SHOWS ROWCOUNT FOR DEV AND VALIDATION SAMPLE
c(nrow(cart.dev), nrow(cart.val))  
length(which(cart.dev$Attrition=="Yes"))/nrow(cart.dev)
length(which(cart.val$Attrition=="Yes"))/nrow(cart.val)

# remove the random variable
cart.dev = subset(cart.dev, select = -random)
cart.val = subset(cart.val, select = -random)

#cart.dev = cart2.dev
#cart.val = cart2.val

library(rpart)
library(rpart.plot)
cartParameters = rpart.control(minsplit=30, minbucket = 10, cp = 0, xval = 15)
cartModel <- rpart(formula = Attrition ~ ., data = cart.dev, method = "class", control = cartParameters)
cartModel

## PRINTING CART MODEL PARAMETERS
library(rattle)
library(RColorBrewer)
fancyRpartPlot(cartModel)
printcp(cartModel)
plotcp(cartModel)

ptree<- prune(cartModel, cp= 0.0028736,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

cart.dev$predict.class <- predict(ptree, cart.dev, type="class")
cart.dev$predict.score <- predict(ptree, cart.dev)
head(cart.dev)
View(cart.dev)

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
};

## deciling
cart.dev$deciles <- decile(cart.dev$predict.score[,2])
View(cart.dev)

## Ranking code
#install.packages("data.table")
library(data.table)
tmp_DT = data.table(cart.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = length(which(Attrition == 'Yes')), 
  cnt_non_resp = length(which(Attrition == 'No'))) ,  
  by=deciles][order(-deciles)];
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);
View(rank)


#install.packages("ROCR")
library(ROCR)
pred <- prediction(cart.dev$predict.score[,2], cart.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#install.packages("ineq")
library(ineq)
gini = ineq(cart.dev$predict.score[,2], type="Gini")

with(cart.dev, table(Attrition, predict.class))
auc
KS
gini


####################################
##VALIDATION FOR HOLDOUT SAMPLE#####
####################################

cart.val$predict.class <- predict(ptree, cart.val, type="class")
cart.val$predict.score <- predict(ptree, cart.val)
head(cart.val)
View(cart.val)
#install.packages(stringr)
#rattle()
#head(CART_DATA.dev$predict.score[,2])
#help("predict")
#View(CART_DATA.dev)


## deciling
cart.val$deciles <- decile(cart.val$predict.score[,2])
View(cart.val)


tmp_DT = data.table(cart.val)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = length(which(Attrition == "Yes")), 
  cnt_non_resp = length(which(Attrition == "No"))) ,
  by=deciles][order(-deciles)];
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);
View(rank)



pred <- prediction(cart.val$predict.score[,2], cart.val$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(cart.val$predict.score[,2], type="Gini")

with(cart.val, table(Attrition, predict.class))
auc
KS
gini

cartNN = subset(cart, select = -random)
cartNN$BusinessTravel = as.integer(cartNN$BusinessTravel)
cartNN$Department = as.integer(cartNN$Department)
cartNN$EducationField = as.integer(cartNN$EducationField)
cartNN$Gender = as.integer(cartNN$Gender)
cartNN$JobRole = as.integer(cartNN$JobRole)
cartNN$MaritalStatus = as.integer(cartNN$MaritalStatus)
cartNN$OverTime = as.integer(cartNN$OverTime)
target = cartNN$Attrition
cartNN = subset(cartNN, select = -Attrition)
library(dummies)

cartNN = dummy.data.frame(cartNN)

cartNN.dev <- cartNN[which(cart$random <= 0.7),] 
cartNN.val <- cartNN[which(cart$random > 0.7),]
cartNN.dev = subset(cartNN, select = -random)

n = make.names(names(cartNN.dev))
#n= subset(n, select = c(-AttritionNo, -AttritionYes))
a <- as.formula(paste('target ~ ' ,paste(n[!n %in% 'Attrition'],collapse='+')))

library(neuralnet)
NNModel1<-neuralnet(formula = a ,
                    data = cartNN.dev, 
                    hidden = c(8,2),
                    err.fct = "ce",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 10,
                    threshold = 0.05,
                    stepmax = 1000)


NNModel1<-neuralnet(formula = Attrition ~ BusinessTravel + Department + Education + 
                      EducationField + EnvironmentSatisfaction + Gender + JobInvolvement + 
                      JobLevel + JobRole + JobSatisfaction + MaritalStatus + OverTime + 
                      PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + 
                      StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance + 
                      YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
                      stability + CompareSal + AgeGroup + DistanceGroup + YearsWithManagerGroup + 
                      WorkYearGroup + NumCompGroup + DailyRateGroup + HourlyRateGroup + 
                      MonthlyRateGroup + MonthlyIncomeGroup ,
                    data = cartNN.dev, 
                    hidden = c(6,2),
                    err.fct = "ce",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 10,
                    threshold = 0.05,
                    stepmax = 1000)


quantile(NNModel1$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
misClassTable = data.frame(Target = cartNN.dev$Attrition, Prediction = NNModel1$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.500,1,0)
with(misClassTable, table(Target, Classification))
sum((misClassTable$Target - misClassTable$Prediction)^2)/2

detach("package:neuralnet", unload=TRUE)

library(ROCR)
names(misClassTable)
pred <- prediction(misClassTable$Prediction, misClassTable$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(misClassTable$Prediction, type="Gini")

with(NNDF.dev, table(misClassTable$Target, misClassTable$Classification))
auc
KS
Gini
