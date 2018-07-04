detach(cart)
cart.dev$set ="dev"
cart.val$set ="val"
NNDF = rbind(cart.dev,cart.val)
attach(NNDF)

NNDF = subset(NNDF, select = c(-predict.class,-predict.score,-deciles ))

#####model.matrix() function for dummy variables############
NNDF =data.frame(NNDF,model.matrix(~BusinessTravel-1))
NNDF =data.frame(NNDF,model.matrix(~Department-1))
NNDF =data.frame(NNDF,model.matrix(~EducationField-1))
#NNDF =data.frame(NNDF,model.matrix(~Gender-1))
NNDF =data.frame(NNDF,model.matrix(~JobRole-1))
NNDF =data.frame(NNDF,model.matrix(~MaritalStatus-1))
NNDF =data.frame(NNDF,model.matrix(~OverTime-1))

######create dummy variables -for numeric categorical#######

NNDF$Education_1  <- ifelse(NNDF$Education == 1,1,0)
NNDF$Education_2  <- ifelse(NNDF$Education == 2,1,0)
NNDF$Education_3  <- ifelse(NNDF$Education == 3,1,0)
NNDF$Education_4  <- ifelse(NNDF$Education == 4,1,0)
NNDF$Education_5  <- ifelse(NNDF$Education == 5,1,0)
NNDF$EnvironmentSatisfaction_1  <- ifelse(NNDF$EnvironmentSatisfaction == 1,1,0)
NNDF$EnvironmentSatisfaction_2  <- ifelse(NNDF$EnvironmentSatisfaction == 2,1,0)
NNDF$EnvironmentSatisfaction_3  <- ifelse(NNDF$EnvironmentSatisfaction == 3,1,0)
NNDF$EnvironmentSatisfaction_4  <- ifelse(NNDF$EnvironmentSatisfaction == 4,1,0)

NNDF$JobInvolvement_1  <- ifelse(NNDF$JobInvolvement == 1,1,0)
NNDF$JobInvolvement_2  <- ifelse(NNDF$JobInvolvement == 2,1,0)
NNDF$JobInvolvement_3  <- ifelse(NNDF$JobInvolvement == 3,1,0)
NNDF$JobInvolvement_4  <- ifelse(NNDF$JobInvolvement == 4,1,0)

NNDF$JobLevel_1  <- ifelse(NNDF$JobLevel == 1,1,0)
NNDF$JobLevel_2  <- ifelse(NNDF$JobLevel == 2,1,0)
NNDF$JobLevel_3  <- ifelse(NNDF$JobLevel == 3,1,0)
NNDF$JobLevel_4  <- ifelse(NNDF$JobLevel == 4,1,0)
NNDF$JobLevel_5  <- ifelse(NNDF$JobLevel == 5,1,0)

NNDF$JobSatisfaction_1  <- ifelse(NNDF$JobSatisfaction == 1,1,0)
NNDF$JobSatisfaction_2  <- ifelse(NNDF$JobSatisfaction == 2,1,0)
NNDF$JobSatisfaction_3  <- ifelse(NNDF$JobSatisfaction == 3,1,0)
NNDF$JobSatisfaction_4  <- ifelse(NNDF$JobSatisfaction == 4,1,0)

# NNDF$PerformanceRating_3  <- ifelse(NNDF$PerformanceRating == 3,1,0)
# NNDF$PerformanceRating_4  <- ifelse(NNDF$PerformanceRating == 4,1,0)

NNDF$RelationshipSatisfaction_1  <- ifelse(NNDF$RelationshipSatisfaction == 1,1,0)
NNDF$RelationshipSatisfaction_2  <- ifelse(NNDF$RelationshipSatisfaction == 2,1,0)
NNDF$RelationshipSatisfaction_3  <- ifelse(NNDF$RelationshipSatisfaction == 3,1,0)
NNDF$RelationshipSatisfaction_4  <- ifelse(NNDF$RelationshipSatisfaction == 4,1,0)

NNDF$StockOptionLevel_0  <- ifelse(NNDF$StockOptionLevel == 0,1,0)
NNDF$StockOptionLevel_1  <- ifelse(NNDF$StockOptionLevel == 1,1,0)
NNDF$StockOptionLevel_2  <- ifelse(NNDF$StockOptionLevel == 2,1,0)
NNDF$StockOptionLevel_3  <- ifelse(NNDF$StockOptionLevel == 3,1,0)

NNDF$WorkLifeBalance_1  <- ifelse(NNDF$WorkLifeBalance == 1,1,0)
NNDF$WorkLifeBalance_2  <- ifelse(NNDF$WorkLifeBalance == 2,1,0)
NNDF$WorkLifeBalance_3  <- ifelse(NNDF$WorkLifeBalance == 3,1,0)
NNDF$WorkLifeBalance_4  <- ifelse(NNDF$WorkLifeBalance == 4,1,0)

NNDF$AgeGroup_1  <- ifelse(NNDF$AgeGroup == 1,1,0)
NNDF$AgeGroup_2  <- ifelse(NNDF$AgeGroup == 2,1,0)
NNDF$AgeGroup_3  <- ifelse(NNDF$AgeGroup == 3,1,0)
NNDF$AgeGroup_4  <- ifelse(NNDF$AgeGroup == 4,1,0)
NNDF$AgeGroup_5  <- ifelse(NNDF$AgeGroup == 5,1,0)
NNDF$AgeGroup_6  <- ifelse(NNDF$AgeGroup == 6,1,0)
NNDF$AgeGroup_7  <- ifelse(NNDF$AgeGroup == 7,1,0)
NNDF$AgeGroup_8  <- ifelse(NNDF$AgeGroup == 8,1,0)

NNDF$DistanceGroup_1  <- ifelse(NNDF$DistanceGroup == 1,1,0)
NNDF$DistanceGroup_2  <- ifelse(NNDF$DistanceGroup == 2,1,0)
NNDF$DistanceGroup_3  <- ifelse(NNDF$DistanceGroup == 3,1,0)
NNDF$DistanceGroup_4  <- ifelse(NNDF$DistanceGroup == 4,1,0)
NNDF$DistanceGroup_5  <- ifelse(NNDF$DistanceGroup == 5,1,0)
NNDF$DistanceGroup_6  <- ifelse(NNDF$DistanceGroup == 6,1,0)

NNDF$YearsWithManagerGroup_1  <- ifelse(NNDF$YearsWithManagerGroup == 1,1,0)
NNDF$YearsWithManagerGroup_2  <- ifelse(NNDF$YearsWithManagerGroup == 2,1,0)
NNDF$YearsWithManagerGroup_3  <- ifelse(NNDF$YearsWithManagerGroup == 3,1,0)
NNDF$YearsWithManagerGroup_4  <- ifelse(NNDF$YearsWithManagerGroup == 4,1,0)
NNDF$YearsWithManagerGroup_5  <- ifelse(NNDF$YearsWithManagerGroup == 5,1,0)

NNDF$WorkYearGroup_1  <- ifelse(NNDF$WorkYearGroup == 1,1,0)
NNDF$WorkYearGroup_2  <- ifelse(NNDF$WorkYearGroup == 2,1,0)
NNDF$WorkYearGroup_3  <- ifelse(NNDF$WorkYearGroup == 3,1,0)
NNDF$WorkYearGroup_4  <- ifelse(NNDF$WorkYearGroup == 4,1,0)
NNDF$WorkYearGroup_5  <- ifelse(NNDF$WorkYearGroup == 5,1,0)
NNDF$WorkYearGroup_6  <- ifelse(NNDF$WorkYearGroup == 6,1,0)
NNDF$WorkYearGroup_7  <- ifelse(NNDF$WorkYearGroup == 7,1,0)
NNDF$WorkYearGroup_8  <- ifelse(NNDF$WorkYearGroup == 8,1,0)
NNDF$WorkYearGroup_9  <- ifelse(NNDF$WorkYearGroup == 9,1,0)

NNDF$NumCompGroup_1  <- ifelse(NNDF$NumCompGroup == 1,1,0)
NNDF$NumCompGroup_2  <- ifelse(NNDF$NumCompGroup == 2,1,0)
NNDF$NumCompGroup_3  <- ifelse(NNDF$NumCompGroup == 3,1,0)

NNDF$DailyRateGroup_1  <- ifelse(NNDF$DailyRateGroup == 1,1,0)
NNDF$DailyRateGroup_2  <- ifelse(NNDF$DailyRateGroup == 2,1,0)
NNDF$DailyRateGroup_3  <- ifelse(NNDF$DailyRateGroup == 3,1,0)
NNDF$DailyRateGroup_4  <- ifelse(NNDF$DailyRateGroup == 4,1,0)
NNDF$DailyRateGroup_5  <- ifelse(NNDF$DailyRateGroup == 5,1,0)
NNDF$DailyRateGroup_6  <- ifelse(NNDF$DailyRateGroup == 6,1,0)

NNDF$HourlyRateGroup_1  <- ifelse(NNDF$HourlyRateGroup == 1,1,0)
NNDF$HourlyRateGroup_2  <- ifelse(NNDF$HourlyRateGroup == 2,1,0)
NNDF$HourlyRateGroup_3  <- ifelse(NNDF$HourlyRateGroup == 3,1,0)
NNDF$HourlyRateGroup_4  <- ifelse(NNDF$HourlyRateGroup == 4,1,0)
NNDF$HourlyRateGroup_5  <- ifelse(NNDF$HourlyRateGroup == 5,1,0)
NNDF$HourlyRateGroup_6  <- ifelse(NNDF$HourlyRateGroup == 6,1,0)
NNDF$HourlyRateGroup_7  <- ifelse(NNDF$HourlyRateGroup == 7,1,0)

NNDF$MonthlyRateGroup_1  <- ifelse(NNDF$MonthlyRateGroup == 1,1,0)
NNDF$MonthlyRateGroup_2  <- ifelse(NNDF$MonthlyRateGroup == 2,1,0)
NNDF$MonthlyRateGroup_3  <- ifelse(NNDF$MonthlyRateGroup == 3,1,0)
NNDF$MonthlyRateGroup_4 <- ifelse(NNDF$MonthlyRateGroup == 4,1,0)
NNDF$MonthlyRateGroup_5  <- ifelse(NNDF$MonthlyRateGroup == 5,1,0)
NNDF$MonthlyRateGroup_6  <- ifelse(NNDF$MonthlyRateGroup == 6,1,0)

NNDF$MonthlyIncomeGroup_1  <- ifelse(NNDF$MonthlyIncomeGroup == 1,1,0)
NNDF$MonthlyIncomeGroup_2  <- ifelse(NNDF$MonthlyIncomeGroup == 2,1,0)
NNDF$MonthlyIncomeGroup_3  <- ifelse(NNDF$MonthlyIncomeGroup == 3,1,0)
NNDF$MonthlyIncomeGroup_4  <- ifelse(NNDF$MonthlyIncomeGroup == 4,1,0)
NNDF$MonthlyIncomeGroup_5  <- ifelse(NNDF$MonthlyIncomeGroup == 5,1,0)
NNDF$MonthlyIncomeGroup_6  <- ifelse(NNDF$MonthlyIncomeGroup == 6,1,0)
NNDF$MonthlyIncomeGroup_7  <- ifelse(NNDF$MonthlyIncomeGroup == 7,1,0)

#########Scaling of continous variables #################################

NNDF$stability  <- scale(NNDF$stability)

NNDF$PercentSalaryHike_S <- scale(NNDF$PercentSalaryHike)

#NNDF$TotalWorkingYears_S <- scale(NNDF$TotalWorkingYears)
NNDF$TrainingTimesLastYear_S <- scale(NNDF$TrainingTimesLastYear)
#NNDF$WorkLifeBalance_S <- scale(NNDF$WorkLifeBalance)
NNDF$YearsAtCompany_S  <- scale(NNDF$YearsAtCompany)
NNDF$YearsInCurrentRole_S  <- scale(NNDF$YearsInCurrentRole)
NNDF$YearsSinceLastPromotion_S <- scale(NNDF$YearsSinceLastPromotion)
#NNDF$YearsWithCurrManager_S <- scale(NNDF$YearsWithCurrManager)


######SEPARATE DATA BASED set values#########

NNDF.dev <-NNDF[which(NNDF$set =="dev"),] 
NNDF.val <-NNDF[which(NNDF$set == "val"),]

#####SHOWS ROWCOUNT FOR DEV AND VALIDATION SAMPLE###########

c(nrow(NNDF.dev), nrow(NNDF.val))  
str(NNDF.dev)
NNDF.dev$Attrition = as.numeric(ifelse(NNDF.dev$Attrition =="Yes",1,0))
library(neuralnet)
NNModel1<-neuralnet(formula = Attrition ~  
                    + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely
                    + DepartmentHuman.Resources + DepartmentResearch...Development + DepartmentSales
                    + Education_1 + Education_2 + Education_3 + Education_4 + Education_5
                    + EducationFieldOther + EducationFieldHuman.Resources + EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical
                    + EnvironmentSatisfaction_1 + EnvironmentSatisfaction_2 +  EnvironmentSatisfaction_3 +  EnvironmentSatisfaction_4
                    + JobInvolvement_1 + JobInvolvement_2 + JobInvolvement_3 + JobInvolvement_4
                    + JobLevel_1 + JobLevel_2 +  JobLevel_3 + JobLevel_4 + JobLevel_5
                    + JobRoleManager + JobRoleSales.Executive + JobRoleHuman.Resources + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Representative + JobRoleLaboratory.Technician + JobRoleManufacturing.Director   + JobRoleHealthcare.Representative
                    + JobSatisfaction_1 + JobSatisfaction_2 + JobSatisfaction_3 + JobSatisfaction_4
                    + MaritalStatusSingle + MaritalStatusMarried + MaritalStatusDivorced
                    + OverTimeNo + OverTimeYes
                    + RelationshipSatisfaction_1 + RelationshipSatisfaction_2 + RelationshipSatisfaction_3 + RelationshipSatisfaction_4
                    + StockOptionLevel_0 + StockOptionLevel_1 + StockOptionLevel_2 + StockOptionLevel_3
                    + WorkLifeBalance_1 + WorkLifeBalance_2 + WorkLifeBalance_3 +WorkLifeBalance_4
                    + AgeGroup_1 + AgeGroup_2 + AgeGroup_3 + AgeGroup_4 + AgeGroup_5 + AgeGroup_6 + AgeGroup_7 + AgeGroup_8
                    + DistanceGroup_1 + DistanceGroup_2 + DistanceGroup_3 + DistanceGroup_4 + DistanceGroup_5 + DistanceGroup_6
                    + YearsWithManagerGroup_1 + YearsWithManagerGroup_2+ YearsWithManagerGroup_3+YearsWithManagerGroup_4 + YearsWithManagerGroup_5
                    + WorkYearGroup_1 + WorkYearGroup_2 + WorkYearGroup_3 + WorkYearGroup_4 + WorkYearGroup_5 + WorkYearGroup_6 + WorkYearGroup_7 + WorkYearGroup_8 + WorkYearGroup_9
                    + NumCompGroup_1 + NumCompGroup_2 +  NumCompGroup_3                    
                    + DailyRateGroup_1 + DailyRateGroup_2 + DailyRateGroup_3 + DailyRateGroup_4 + DailyRateGroup_5 + DailyRateGroup_6             
                    + HourlyRateGroup_1 + HourlyRateGroup_2 + HourlyRateGroup_3 + HourlyRateGroup_4 + HourlyRateGroup_5 + HourlyRateGroup_6 + HourlyRateGroup_7
                    + MonthlyRateGroup_1 + MonthlyRateGroup_2 + MonthlyRateGroup_3 + MonthlyRateGroup_4 + MonthlyRateGroup_5 + MonthlyRateGroup_6 
                    + MonthlyIncomeGroup_1 + MonthlyIncomeGroup_2 + MonthlyIncomeGroup_3 + MonthlyIncomeGroup_4 + MonthlyIncomeGroup_5 + MonthlyIncomeGroup_6 + MonthlyIncomeGroup_7
                    + TrainingTimesLastYear_S  + YearsAtCompany_S + YearsInCurrentRole_S + YearsSinceLastPromotion_S
                    + stability + PercentSalaryHike_S  ,
                      data = NNDF.dev, 
                    hidden = c(9,3),
                    err.fct = "sse",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 10,
                    threshold = 0.1,
                    stepmax = 2000)

# Neural net with cart parameters
NNModel1<-neuralnet(formula = Attrition ~  
                    + BusinessTravelNon.Travel + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely
                    + EducationFieldOther + EducationFieldHuman.Resources + EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical
                    + EnvironmentSatisfaction_1 + EnvironmentSatisfaction_2 +  EnvironmentSatisfaction_3 +  EnvironmentSatisfaction_4
                    + JobInvolvement_1 + JobInvolvement_2 + JobInvolvement_3 + JobInvolvement_4
                    + JobLevel_1 + JobLevel_2 +  JobLevel_3 + JobLevel_4 + JobLevel_5
                    + JobRoleManager + JobRoleSales.Executive + JobRoleHuman.Resources + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Representative + JobRoleLaboratory.Technician + JobRoleManufacturing.Director   + JobRoleHealthcare.Representative
                    + JobSatisfaction_1 + JobSatisfaction_2 + JobSatisfaction_3 + JobSatisfaction_4
                    + OverTimeNo + OverTimeYes
                    + StockOptionLevel_0 + StockOptionLevel_1 + StockOptionLevel_2 + StockOptionLevel_3
                    + WorkLifeBalance_1 + WorkLifeBalance_2 + WorkLifeBalance_3 +WorkLifeBalance_4
                    + DistanceGroup_1 + DistanceGroup_2 + DistanceGroup_3 + DistanceGroup_4 + DistanceGroup_5 + DistanceGroup_6
                    + WorkYearGroup_1 + WorkYearGroup_2 + WorkYearGroup_3 + WorkYearGroup_4 + WorkYearGroup_5 + WorkYearGroup_6 + WorkYearGroup_7 + WorkYearGroup_8 + WorkYearGroup_9
                    + DailyRateGroup_1 + DailyRateGroup_2 + DailyRateGroup_3 + DailyRateGroup_4 + DailyRateGroup_5 + DailyRateGroup_6             
                    + TrainingTimesLastYear_S  + YearsAtCompany_S   + YearsSinceLastPromotion_S
                    + stability  + PercentSalaryHike_S  ,
                    data = NNDF.dev, 
                    hidden = c(8,3),
                    err.fct = "sse",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 10,
                    threshold = 0.1,
                    stepmax = 2000)
plot(NNModel1)

NNDF.dev$Predict.score = NNModel1$net.result[[1]] 
quantile(NNModel1$net.result[[1]], c(0,10,20,30,40,50,60,70,80,90,100)/100)
misClassTable = data.frame(Target = NNDF.dev$Attrition, Prediction = NNModel1$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.500,1,0)
with(misClassTable, table(Target, Classification))
sum((misClassTable$Target - misClassTable$Prediction)^2)/2

misClassTable$deciles <- decile(misClassTable$Prediction)


## Ranking code
tmp_DT = data.table(misClassTable)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)

rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)
View(rank)

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
gini
####################################################################
#############NN MODEL VALIDATION on Test DATA###################
###################################################################
rowSums(is.na(NNDF.val)) ##check for missing values in rows
colSums(is.na(NNDF.val))

NNDF.val$Attrition = as.numeric(ifelse(NNDF.val$Attrition =="Yes",1,0))

x <- subset(NNDF.val, 
            select = c(BusinessTravelNon.Travel,BusinessTravelTravel_Frequently,BusinessTravelTravel_Rarely
                       ,DepartmentHuman.Resources,DepartmentResearch...Development,DepartmentSales
                       ,Education_1,Education_2,Education_3,Education_4,Education_5
                       ,EducationFieldOther,EducationFieldHuman.Resources,EducationFieldLife.Sciences,EducationFieldMarketing,EducationFieldMedical
                       ,EnvironmentSatisfaction_1,EnvironmentSatisfaction_2, EnvironmentSatisfaction_3, EnvironmentSatisfaction_4
                       ,JobInvolvement_1,JobInvolvement_2,JobInvolvement_3,JobInvolvement_4
                       ,JobLevel_1,JobLevel_2, JobLevel_3,JobLevel_4,JobLevel_5
                       ,JobRoleManager,JobRoleSales.Executive,JobRoleHuman.Resources,JobRoleResearch.Director,JobRoleResearch.Scientist,JobRoleSales.Representative,JobRoleLaboratory.Technician,JobRoleManufacturing.Director  ,JobRoleHealthcare.Representative
                       ,JobSatisfaction_1,JobSatisfaction_2,JobSatisfaction_3,JobSatisfaction_4
                       ,MaritalStatusSingle,MaritalStatusMarried,MaritalStatusDivorced
                       ,OverTimeNo,OverTimeYes
                       ,RelationshipSatisfaction_1,RelationshipSatisfaction_2,RelationshipSatisfaction_3,RelationshipSatisfaction_4
                       ,StockOptionLevel_0,StockOptionLevel_1,StockOptionLevel_2,StockOptionLevel_3
                       ,WorkLifeBalance_1,WorkLifeBalance_2,WorkLifeBalance_3 ,WorkLifeBalance_4
                       ,AgeGroup_1,AgeGroup_2,AgeGroup_3,AgeGroup_4,AgeGroup_5,AgeGroup_6,AgeGroup_7,AgeGroup_8
                       ,DistanceGroup_1,DistanceGroup_2,DistanceGroup_3,DistanceGroup_4,DistanceGroup_5,DistanceGroup_6
                       ,YearsWithManagerGroup_1,YearsWithManagerGroup_2, YearsWithManagerGroup_3,YearsWithManagerGroup_4,YearsWithManagerGroup_5
                       ,WorkYearGroup_1,WorkYearGroup_2,WorkYearGroup_3,WorkYearGroup_4,WorkYearGroup_5,WorkYearGroup_6,WorkYearGroup_7,WorkYearGroup_8,WorkYearGroup_9
                       ,NumCompGroup_1,NumCompGroup_2, NumCompGroup_3                    
                       ,DailyRateGroup_1,DailyRateGroup_2,DailyRateGroup_3,DailyRateGroup_4,DailyRateGroup_5,DailyRateGroup_6             
                       ,HourlyRateGroup_1,HourlyRateGroup_2,HourlyRateGroup_3,HourlyRateGroup_4,HourlyRateGroup_5,HourlyRateGroup_6,HourlyRateGroup_7
                       ,MonthlyRateGroup_1,MonthlyRateGroup_2,MonthlyRateGroup_3,MonthlyRateGroup_4,MonthlyRateGroup_5,MonthlyRateGroup_6 
                       ,MonthlyIncomeGroup_1,MonthlyIncomeGroup_2,MonthlyIncomeGroup_3,MonthlyIncomeGroup_4,MonthlyIncomeGroup_5,MonthlyIncomeGroup_6,MonthlyIncomeGroup_7
                       ,TrainingTimesLastYear_S ,YearsAtCompany_S,YearsInCurrentRole_S,YearsSinceLastPromotion_S
                       ,stability,PercentSalaryHike_S ))

# with cart parameters
x <- subset(NNDF.val, 
            select = c(BusinessTravelNon.Travel,BusinessTravelTravel_Frequently,BusinessTravelTravel_Rarely
                       ,EducationFieldOther,EducationFieldHuman.Resources,EducationFieldLife.Sciences,EducationFieldMarketing,EducationFieldMedical
                       ,EnvironmentSatisfaction_1,EnvironmentSatisfaction_2, EnvironmentSatisfaction_3, EnvironmentSatisfaction_4
                       ,JobInvolvement_1,JobInvolvement_2,JobInvolvement_3,JobInvolvement_4
                       ,JobLevel_1,JobLevel_2, JobLevel_3,JobLevel_4,JobLevel_5
                       ,JobRoleManager,JobRoleSales.Executive,JobRoleHuman.Resources,JobRoleResearch.Director,JobRoleResearch.Scientist,JobRoleSales.Representative,JobRoleLaboratory.Technician,JobRoleManufacturing.Director  ,JobRoleHealthcare.Representative
                       ,JobSatisfaction_1,JobSatisfaction_2,JobSatisfaction_3,JobSatisfaction_4
                       ,OverTimeNo,OverTimeYes
                       ,StockOptionLevel_0,StockOptionLevel_1,StockOptionLevel_2,StockOptionLevel_3
                       ,WorkLifeBalance_1,WorkLifeBalance_2,WorkLifeBalance_3 ,WorkLifeBalance_4
                       ,DistanceGroup_1,DistanceGroup_2,DistanceGroup_3,DistanceGroup_4,DistanceGroup_5,DistanceGroup_6
                       ,WorkYearGroup_1,WorkYearGroup_2,WorkYearGroup_3,WorkYearGroup_4,WorkYearGroup_5,WorkYearGroup_6,WorkYearGroup_7,WorkYearGroup_8,WorkYearGroup_9
                       ,DailyRateGroup_1,DailyRateGroup_2,DailyRateGroup_3,DailyRateGroup_4,DailyRateGroup_5,DailyRateGroup_6             
                       ,TrainingTimesLastYear_S ,YearsAtCompany_S,YearsSinceLastPromotion_S
                       ,stability,PercentSalaryHike_S ))

library(neuralnet)
TestOutput = compute(NNModel1, x)     #######COMPUTE FUNCTION TO TEST MODEL ON TEST DATA###############################
#TestOutput$neurons

NNDF.val$Predict.score = TestOutput$net.result      
quantile(TestOutput$net.result, c(0,10,20,30,40,50,60,70,80,90,100)/100)
TestmisClassTable = data.frame(Target = NNDF.val$Attrition, TestPrediction = TestOutput$net.result )
TestmisClassTable$Classification = ifelse(TestmisClassTable$TestPrediction>0.500,1,0)
#head(TestmisClassTable$Classification)
with(TestmisClassTable, table(Target, Classification))
sum((TestmisClassTable$Target - TestmisClassTable$TestPrediction)^2)/2
## deciling
TestmisClassTable$deciles <- decile(TestmisClassTable$TestPrediction)
## Ranking code
tmp_DT = data.table(TestmisClassTable)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);

rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

detach("package:neuralnet", unload=TRUE)
pred <- prediction(TestmisClassTable$TestPrediction, TestmisClassTable$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(TestmisClassTable$TestPrediction, type="Gini")

with(NNDF.val, table(TestmisClassTable$Target, TestmisClassTable$Classification))
auc
KS
gini


#Ensemble modelling

ensembleDataFrame = as.data.frame(cbind(cart.dev$predict.score[,2],NNDF.dev$Predict.score,NNDF.dev$Attrition))
colnames(ensembleDataFrame) = c("cartScore","NnetScore", "Target")
ensembleDataFrame$AvgScore = (ensembleDataFrame$cartScore+ensembleDataFrame$NnetScore)/2
ensemblemisClassTable = data.frame(Target = ensembleDataFrame$Target, TestPrediction = ensembleDataFrame$AvgScore )


ensemblemisClassTable$Classification = ifelse(ensemblemisClassTable$TestPrediction>0.500,1,0)
#head(ensemblemisClassTable$Classification)
with(ensemblemisClassTable, table(Target, Classification))
sum((ensemblemisClassTable$Target - ensemblemisClassTable$TestPrediction)^2)/2
## deciling
ensemblemisClassTable$deciles <- decile(ensemblemisClassTable$TestPrediction)
## Ranking code
tmp_DT = data.table(ensemblemisClassTable)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);

rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

pred <- prediction(ensemblemisClassTable$TestPrediction, ensemblemisClassTable$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(ensemblemisClassTable$TestPrediction, type="Gini")

with(ensembleDataFrame, table(ensemblemisClassTable$Target, ensemblemisClassTable$Classification))
auc
KS
gini


ensembleDataFrame = as.data.frame(cbind(cart.val$predict.score[,2],NNDF.val$Predict.score,NNDF.val$Attrition))
colnames(ensembleDataFrame) = c("cartScore","NnetScore", "Target")
ensembleDataFrame$AvgScore = (ensembleDataFrame$cartScore+ensembleDataFrame$NnetScore)/2
ensemblemisClassTable = data.frame(Target = ensembleDataFrame$Target, TestPrediction = ensembleDataFrame$AvgScore )


ensemblemisClassTable$Classification = ifelse(ensemblemisClassTable$TestPrediction>0.500,1,0)
#head(ensemblemisClassTable$Classification)
with(ensemblemisClassTable, table(Target, Classification))
sum((ensemblemisClassTable$Target - ensemblemisClassTable$TestPrediction)^2)/2
## deciling
ensemblemisClassTable$deciles <- decile(ensemblemisClassTable$TestPrediction)
## Ranking code
tmp_DT = data.table(ensemblemisClassTable)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);

rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)


pred <- prediction(ensemblemisClassTable$TestPrediction, ensemblemisClassTable$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(ensemblemisClassTable$TestPrediction, type="Gini")

with(ensembleDataFrame, table(ensemblemisClassTable$Target, ensemblemisClassTable$Classification))
auc
KS
gini


# Stacking ensemble
library(caretEnsemble)
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('rpart', 'nnet','avNNet')
# #set.seed(seed)
models <- caretList(Attrition~., data=cart, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

