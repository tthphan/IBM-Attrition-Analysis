########################################
########################################
##... Employee Attrition prediction
########################################
########################################

###Packages installation

library("dplyr")
library("ggplot2")
library("caret")
library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("randomForest")
library("xgboost")
library("pdp")
library("stargazer")
library("ggtext")
library("corrplot")
library("gt")
library("stringr")

###################
##... Read the data
###################

getwd() 

setwd("D:/UOW Resources/Exchange/3185/Attrition")

df <- read.csv("IBM_employee_attrition.csv", header = TRUE, as.is = TRUE, stringsAsFactors = FALSE,
               check.names = FALSE)

View(df)

##Data Transformation

#Seniority
df$Seniority[df$TotalWorkingYears==0] <- "Newcomers"
df$Seniority[df$TotalWorkingYears>=1 & df$TotalWorkingYears<=2] <- "Juniors"
df$Seniority[df$TotalWorkingYears>2 & df$TotalWorkingYears<=5] <- "Associates"
df$Seniority[df$TotalWorkingYears>5 & df$TotalWorkingYears<=10] <- "Mid-levels"
df$Seniority[df$TotalWorkingYears>10] <- "Seniors"

#Level Match
df$LevelMatch[df$Education == df$JobLevel] <- 0
df$LevelMatch[df$Education > df$JobLevel] <- 1
df$LevelMatch[df$Education < df$JobLevel] <- -1

#QualificationMatch (where an employee's qualification is not align with their work field)

df$Skills[df$EducationField=="Life Sciences" ] <- "1"
df$Skills[df$EducationField=="Medical" ] <- "2"
df$Skills[df$EducationField=="Marketing" ] <- "3"
df$Skills[df$EducationField=="Technical Degree" ] <- "4"
df$Skills[df$EducationField=="Human Resources" ] <- "5"
df$Skills[df$EducationField=="Other" ] <- "6"

df$QualificationMatch[df$JobRole=="Sales Executive"] <- ifelse(df$Skills == 3 | df$Skills == 4, "1","0")
df$QualificationMatch[df$JobRole=="Research Scientist"] <- ifelse(df$Skills == 1 | df$Skills == 4, "1","0")
df$QualificationMatch[df$JobRole=="Laboratory Technician"] <- ifelse(df$Skills == 4, "1","0")
df$QualificationMatch[df$JobRole=="Manufacturing Director"] <- ifelse(df$Skills == 1 | df$Skills == 4, "1","0")
df$QualificationMatch[df$JobRole=="Healthcare Representative"] <- ifelse(df$Skills == 2, "1","0")
df$QualificationMatch[df$JobRole=="Manager"] <- "1"
df$QualificationMatch[df$JobRole=="Sales Representative"] <- ifelse(df$Skills == 3 | df$Skills == 4, "1","0")
df$QualificationMatch[df$JobRole=="Research Director"] <- ifelse(df$Skills == 1 | df$Skills == 4, "1","0")
df$QualificationMatch[df$JobRole=="Human Resources"] <- ifelse(df$Skills == 5, "1","0")

#Retirement risks
df$RetirementRisk[df$Age >= 55] <- 2
df$RetirementRisk[df$Age >= 50 & df$Age <55] <- 1
df$RetirementRisk[df$Age < 50] <- 0

#YrsExp
df$YrsExp[df$YearsInCurrentRole <=5] <- 0
df$YrsExp[df$YearsInCurrentRole <=10 & df$YearsInCurrentRole>5] <- 1
df$YrsExp[df$YearsInCurrentRole > 10] <- 2

#Position (The Director, Executive, and Management roles are crucial and have higher requirements)
df$Position <- 0
df$Position[df$JobRole == "Sales Executive" | df$JobRole == "Manager" |
              df$JobRole == "Manufacturing Director" | df$JobRole == "Research Director"] <- 1

df$Seniority[df$Seniority == "Newcomers"] <- 1
df$Seniority[df$Seniority == "Juniors"] <- 2
df$Seniority[df$Seniority == "Associates"] <- 3
df$Seniority[df$Seniority == "Mid-levels"] <- 4
df$Seniority[df$Seniority == "Seniors"] <- 5

df$Seniority <- as.numeric(df$Seniority)

df$Gender <- ifelse(df$Gender == "Male",1,0) 

df$QualificationMatch <- as.numeric(df$QualificationMatch)

df$OverTime <- ifelse(df$OverTime == "Yes",1,0) 


##Key Employees Dashboard
#Key Employees ~ Seniority + PerformanceRating + Yrsexp +Position + JobLevel

df$KeyEmployees <- 0.2*(df$PerformanceRating/max(df$PerformanceRating) + df$YrsExp/max(df$YrsExp) +
                          df$Seniority/max(df$Seniority) + df$Position/max(df$Position) + df$JobLevel/max(df$JobLevel))

KeyEmployeeDashboard <- df[,c("KeyEmployees","EmployeeNumber","Attrition", "JobRole","JobLevel","PerformanceRating","Seniority","YearsInCurrentRole","RetirementRisk")]

KeyEmployeeDashboard <- KeyEmployeeDashboard %>% arrange(desc(KeyEmployees))

KeyEmployeeDashboard %>% 
  head(6) %>% 
  gt() %>% 
  tab_header(title = "Key Employees")

##Subset that we interested in 

ds <- df[,c("Attrition","KeyEmployees","Age","Gender","DistanceFromHome","Seniority","RetirementRisk","QualificationMatch","LevelMatch","JobLevel", "JobRole",
            "MonthlyIncome","OverTime", "JobInvolvement","PerformanceRating","PercentSalaryHike","TrainingTimesLastYear","EnvironmentSatisfaction",
            "JobSatisfaction","RelationshipSatisfaction","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
            "YearsWithCurrManager")]

## Analyse

#Attrition rate group by Job Role in each Department

prop_AttDepJR <- df %>% 
  count(Department, Attrition, JobRole) %>% # Count occurrences 
  group_by(JobRole, Department) %>% 
  mutate(Percent = n / sum(n) * 100) 
  
ggplot(prop_AttDepJR, aes(x = JobRole, y = n, fill = Attrition)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.6) + 
  facet_grid(. ~ Department, scales = "free_x", space = "free_x", switch = "x") +
  labs(title = "Attrition Proportion by Job Role and Department", 
       x = "Job Role", 
       y = "Number of Employees") + 
  geom_text(aes(label = ifelse(Attrition == "Yes", "", sprintf("%.1f%%", Percent))), 
            position = position_stack(vjust = 0.5), size = 3) + 
  theme(legend.position ="bottom", 
        axis.title = element_text(size = 13), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(color = "black", size = 10), 
        plot.title = element_text(size = 16), 
        plot.margin = unit(c(0.5,1,1,1), "cm")) + 
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "darkblue")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(breaks = seq(0, 400, by = 50)) 


## Determine the significant predictors of employee attrition

ds$Attrition <- ifelse(ds$Attrition == "Yes", 1, 0)

##Demographics

#Correlation matrix

df1 <-  ds[,c("Attrition", "Age", "Gender", "DistanceFromHome","Seniority", "RetirementRisk")]

correlation1 <- as.matrix(cor(df1))

corrplot(correlation1, method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 0, tl.cex = 0.50)

# Regressions

lreg1 <- glm(Attrition ~ ., data = df1, family = "binomial")

lreg1_red <- glm(Attrition ~ Age + DistanceFromHome + Seniority + RetirementRisk, 
               data = df1, family = "binomial") #Remove Gender

summary(lreg1_red)

anova(lreg1_red, lreg1, test ='LRT')


# Satisfaction & Engagement 

# Correlation matrix
df2 <- ds[,c("Attrition","EnvironmentSatisfaction","JobSatisfaction",
             "RelationshipSatisfaction" ,"WorkLifeBalance",
             "YearsAtCompany","YearsWithCurrManager")]

correlation2 <- as.matrix(cor(df2))

corrplot(correlation2, method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 0, tl.cex = 0.50)

#Regressions
lreg2 <- glm(Attrition ~ ., data =  df2, family = "binomial")

lreg2_red <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction +
                   RelationshipSatisfaction + WorkLifeBalance + YearsWithCurrManager, 
                 data = df2, 
                 family = "binomial") #Remove YearsAtCompany

lreg2_red1 <- glm(Attrition ~ EnvironmentSatisfaction + JobSatisfaction +
                   WorkLifeBalance + YearsWithCurrManager, 
                 data = df2, 
                 family = "binomial") #Remove YearsAtCompany & RelationshipSatisfaction

summary(lreg2_red1)
stargazer(lreg2_red1, type = "html", out = "lreg2_red1.html")

anova(lreg2_red1, lreg2_red, test = 'LRT')


#Job Conditions & Performance

df3 <- ds[,c("Attrition","QualificationMatch","LevelMatch" ,"JobLevel","JobRole", "MonthlyIncome", "OverTime", 
             "JobInvolvement","PerformanceRating","PercentSalaryHike", "TrainingTimesLastYear", 
             "YearsInCurrentRole","YearsSinceLastPromotion")]

#Correlation matrix

correlation3 <- as.matrix(cor(df3[,c(-5)]))

corrplot(correlation3, method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.cex = 0.80, tl.srt = 45)

#Regressions

lreg3 <- glm(Attrition ~ ., data = df3, family = "binomial")

lreg3_jr <- glm(Attrition ~ . - JobRole, data = df3, family = "binomial")

lreg3_red <- glm(Attrition ~ . - MonthlyIncome, data = df3, family = "binomial") #Remove MonthlyIncome

lreg3_red1 <- glm(Attrition ~ . - MonthlyIncome - QualificationMatch,
                  data = df3, family = "binomial") #Remove MonthlyIncome & QualificationMatch

lreg3_red2 <- glm(Attrition ~ . - MonthlyIncome - QualificationMatch - LevelMatch,
                  data = df3, family = "binomial") #Remove LevelMatch, MonthlyIncome & QualificationMatch

lreg3_red3 <- glm(Attrition ~ . - MonthlyIncome - QualificationMatch - LevelMatch - PerformanceRating,
                  data = df3, family = "binomial") #Remove LevelMatch, PerformanceRating, MonthlyIncome & QualificationMatch

lreg3_red4 <- glm(Attrition ~ JobLevel + JobRole + MonthlyIncome + OverTime + JobInvolvement + TrainingTimesLastYear + YearsInCurrentRole + YearsInCurrentRole,
                  data = df3, family = "binomial") #Remove LevelMatch, PerformanceRating, PercentSalaryHike, MonthlyIncome & QualificationMatch

lreg3_red5 <- glm(Attrition ~ . - MonthlyIncome - QualificationMatch - LevelMatch - PerformanceRating - PercentSalaryHike - JobLevel,
                  data = df3, family = "binomial") #Remove LevelMatch, PerformanceRating, PercentSalaryHike, MonthlyIncome, QualificationMatch & JobLevel   

summary(lreg3_red4)
stargazer(lreg3_red4, type = "html", out = "lreg3_red4.html")

anova(lreg3_red5,lreg3_red4, test = 'LRT') #lreg3_red4 still the best


## Compare three final models

anova(lreg2_red1, lreg1_red, lreg3_red4, test = 'LRT' )

# Compute OR and 95% CI to easily interpret the regression model

round(exp(cbind(OR = coef(lreg3_red4), confint(lreg3_red4))), 3)


##... Model Accuracy comparison

#Logistics reg
predictions <- predict(lreg3_red4,
                       newdata = select(df3, -Attrition),
                       type = "response")

pred.attr <- ifelse(predictions < 0.48, 0, 1)

data.frame(R2 = R2(predictions, df3$Attrition),
           RMSE = RMSE(predictions, df3$Attrition),
           MAE = MAE(predictions, df3$Attrition))

glm.cm <- confusionMatrix(as.factor(pred.attr), as.factor(df3$Attrition), positive = "1")

#KNN
knn.train <- train(as.factor(Attrition) ~ . ,
                   data = df3,
                   method = "knn",
                   trControl=trainControl(method = "cv",
                                number = 5))

knn.train

knn.pred <- predict(knn.train, newdata = select(df3, -Attrition))
knn.cm <- confusionMatrix(knn.pred, as.factor(df3$Attrition), positive = "1")

#XGBoost

xgb.cv <- train(as.factor(Attrition) ~ . , 
                data = df3, 
                method = 'xgbTree',
                verbosity = 0,
                trControl=trainControl(method = "cv", 
                                       number = 5))
xgb.cv

xgb.pred <- predict(xgb.cv , newdata = select(df3, -Attrition))
xgb.cm <- confusionMatrix(xgb.pred, as.factor(df3$Attrition), positive = "1")

#Model compare

model_compare <- data.frame(Model = c('Logistics Model',
                                      'KNN',
                                      'Extreme Gradient Boosting'),
                            Accuracy = c(glm.cm$overall[1],
                                         knn.cm$overall[1],
                                         xgb.cm$overall[1]),
                            Sensitivity = c(glm.cm$byClass["Sensitivity"],
                                            knn.cm$byClass["Sensitivity"],
                                            xgb.cm$byClass["Sensitivity"]))

library("tidyr")

model_long <- model_compare %>% 
  pivot_longer(cols = c(Accuracy, Sensitivity), 
               names_to = "Metric", 
               values_to = "Value") 

ggplot(model_long, aes(x = Model, y = Value, fill = Metric)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.7), 
           width = 0.6) + 
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.25, color = "black", size = 4) + 
  labs( x = "Models", 
        y = "Metric Value", 
        title = "Accuracy and Sensitivity of Models (Cross-Validation)" ) + 
  scale_fill_manual(values = c("Accuracy" = "#198038", "Sensitivity" = "#d02670")) + 
  theme(axis.text.y = element_text(size = 10, color = "black")) +
  theme_bw()

### Prediction using the Logistics Model

#Evaluate on the current employees (Attrition = 0) in one more year

pd <- filter(df, Attrition == "No" )

pd$YearsInCurrentRole <- pd$YearsInCurrentRole + 1
pd$YearsSinceLastPromotion <- pd$YearsSinceLastPromotion + 1
pd$YearsAtCompany <- pd$YearsAtCompany + 1
pd$YearsWithCurrManager <- pd$YearsWithCurrManager + 1

predictAttrition <- function(newdata){
  predict(lreg3_red4, newdata = pd, type = 'response') #apply the logistic function
}

pd$Prediction <- ifelse(predictAttrition(pd) <0.50, 0, 1)

PredictionDB <- pd[pd$Prediction == 1,c("Prediction","EmployeeNumber","JobRole","MonthlyIncome","KeyEmployees","RetirementRisk",
                                   "Seniority","PerformanceRating","JobLevel","OverTime","JobInvolvement", 
                                   "TrainingTimesLastYear","YearsInCurrentRole","YearsSinceLastPromotion")]

PredictionDB <- PredictionDB %>% arrange(desc(KeyEmployees))

KeyEmployeeDashboard$Prediction[KeyEmployeeDashboard$Attrition == "No"] <- pd$Prediction
KeyEmployeeDashboard$Prediction[KeyEmployeeDashboard$Attrition == "Yes"] <- "N/A"

#Sort Prediction by key employees and make a clean table of key employees
PredictionDB <- PredictionDB %>% arrange(desc(KeyEmployees))

PredictionDB %>% 
  filter(KeyEmployees > 0.50) %>% 
  gt() %>% 
  tab_header(title = "Key Employees Flight Risk")

##Prescriptions

results <- list()

maxtime <- 100

for (i in 1:nrow(PredictionDB)) {
  employee <- PredictionDB[i, ]
  original_time <- employee$TrainingTimesLastYear
  flipped <- FALSE
  
  for (t in original_time:maxtime) {
    employee$TrainingTimesLastYear <- t
    pred <- predict(lreg3_red4, newdata = employee, type = "response")
    
    if (pred < 0.47) {
      results[[i]] <- data.frame(
        EmployeeNumber = employee$EmployeeNumber,
        OriginalTrainingTimes = original_time,
        MinTrainingTime = t
      )
    flipped <- TRUE
    break
    }
  }
  
  if(!flipped) {
    results[[i]] <- data.frame(
      EmployeeNumber = employee$EmployeeNumber,
      OGT = original_time,
      MinTrainingTime = NA
    )
  }
}

minTrainingTimes <- do.call(rbind,results)

minTrainingTimes %>% 
  gt() %>% 
  tab_header(title = "Minimum Training Needed")
