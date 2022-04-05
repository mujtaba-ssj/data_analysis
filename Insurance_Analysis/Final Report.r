#####Initial report####

library(dplyr)
library(ggplot2)
library(relaimpo)
library(corrplot)
library(tidyverse)
library(caret)
library(psych)
library(ggcorrplot)

setwd("C:\\Users\\muj_m\\Desktop\\aly_6015\\Major project\\Insurence_Cost")
insurance <- read.csv("insurance.csv")
class(insurance$region)
head(insurance)
str(insurance)
dim(insurance)
summary(insurance$charges)
summary(insurance$age)
summary(insurance$sex)
summary(insurance$smoker)
summary(insurance$bmi)
summary(insurance$children)
class(insurance$age)
typeof(insurance$age)

#checking if we have any empty values
sum(is.na(insurance))

summary(insurance)

#checking the variable datatype we have
sapply(insurance,class)

#converting sex and smoker into factor
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)

#regions within our dataset
unique(insurance$region)
dt <- data.frame(table(insurance$region))

#Ages within our dataset
insurance1 <- insurance[order(insurance$age,decreasing = FALSE),]
ages <- unique(insurance1$age)

#histogram of age, bmi & childs
par(mfrow = c(1,3))
hist(insurance$age, main = "Histogram of age", col = "lightgreen", xlab = "age", ylab= "count")
hist(insurance$bmi, main = "Histogram of bmi", col = "lightgreen", xlab = "bmi", ylab= "count")
hist(insurance$children, main = "Histogram of children", col = "lightgreen", xlab = "children", ylab= "count")

#knowing which variable in important
model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance)
summary(model)
relative_importance <- calc.relimp(model, type = "lmg", rela = TRUE)
sort(relative_importance$lmg, decreasing = TRUE)

## Summarize medical expenses
summary(insurance$charges)

## Correlation matrix
cor(insurance[c("age", "bmi", "children", "charges")])


model <- lm(charges ~ sex, data = insurance)
summary(model)$coef
model

#Interpretations

# 1. Does smoking affect the insurance price?


boxplot(insurance$charges ~ insurance$smoker,main="Box plot of smoking records in terms of Insurance Charges",
        xlab = "Smoking Record", ylab = "Insurance Charges")



ggplot(insurance, aes(x=charges, y=bmi, color = smoker)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method = "lm")




# 2.Percentage of males vs females who have opted for insurance?



insurance %>% ggplot(aes(x = '', y = ..count.., fill = insurance$sex)) +
  geom_bar() + coord_polar('y', start = 0)



Total_count <- table(insurance$sex)
Total_count



# 3. Can a dummy variable or subset be created within this data set?



MALE <- subset(insurance, insurance$sex == "male")
headtail(MALE)



FEMALE <- subset(insurance, insurance$sex == "female")
headtail(FEMALE)




histo1 <-hist(MALE$bmi, ylab = "Frequency", xlab = "BMI", main = "MALE BMI FREQUENCY HISTOGRAM",
              xlim = c(10,60), ylim = c(0,250), col = "orange", col.main ="blue")
xfit<-seq(min(MALE$bmi),max(MALE$bmi),length=40)
yfit<-dnorm(xfit,mean=mean(MALE$bmi),sd=sd(MALE$bmi))
yfit <- yfit*diff(histo1$mids[1:2])*length(MALE$bmi)
lines(xfit, yfit, col="blue", lwd=2)




histo2 <-hist(FEMALE$bmi, ylab = "Frequency", xlab = "BMI", main = "FEMALE BMI FREQUENCY HISTOGRAM",
              xlim = c(10,60), ylim = c(0,250), col = "orange", col.main ="blue")
xfit<-seq(min(FEMALE$bmi),max(FEMALE$bmi),length=40)
yfit<-dnorm(xfit,mean=mean(FEMALE$bmi),sd=sd(FEMALE$bmi))
yfit <- yfit*diff(histo2$mids[1:2])*length(FEMALE$bmi)
lines(xfit, yfit, col="blue", lwd=2)



summary(MALE)
summary(FEMALE)



# 4. Are there any variables that influence price?

Correlation_matrix <-cor(insurance[sapply(insurance,is.numeric)])
Correlation_matrix
ggcorrplot(Correlation_matrix,method = "square",tl.cex = 10, tl.srt = 100)


#linear model for smoking and charges
lm(charges~smoker, data=insurance)

#region affecting price (region does not have much impact on the charges)
ggplot(data = insurance,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges per Region")

#children affecting price
ggplot(data = insurance,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Boxplot of Medical Charges by Number of Children")

##### draft report #####


#chi-square
table3 <- table("smoker" =insurance$smoker, "charges"=insurance$charges)
View(table3)

table4 <- table("sex" = insurance$sex,"smoker" = insurance$smoker)
View(table4)

#h0: gender and smoker are independent of one another
#h1: gender and smoker are dependent on one another
#critical value = 0.05
ch2 <- chisq.test(table4)
ch2
alpha <- 0.05
ifelse(ch2$p.value>alpha,"Fail to reject Null Hypothesis","Reject Null Hypothesis")

#h0: smoker affects the charges
#h1: smoker does not affects the charges
#critical value = 0.05
ch1 <- chisq.test(table3)
ch1
alpha <- 0.05
ifelse(ch1$p.value>alpha,"Fail to reject Null Hypothesis","Reject Null Hypothesis")


#### method 1 Linear regression####
#splitting on 80%
n_train <- round(0.8 * nrow(insurance))
trainIndex <- sample(1:nrow(insurance), n_train)
train <- insurance[trainIndex, ]
test <- insurance[-trainIndex, ]

#linear model
model1<- lm(charges ~ age + sex + bmi + children + smoker + region, data=train)
summary(model1)

#getting statistics
r2 <- summary(model1)$r.squared
r2

#predicting data on test
pred <- predict(model1, newdata=test)

#rmse
rmse1 <- RMSE(test$charges,pred)
rmse1

#creating model without sex

model2 <- lm(charges ~ age + bmi + children + smoker + region, data=train)
summary(model2)

#getting statistics
r2_1 <- summary(model2)$r.squared
r2_1

#predicting data on test
pred1 <- predict(model2, newdata=test)

#rmse
rmse2 <- RMSE(test$charges,pred1)
rmse2

#compare statistics for above linear and select which is best
#### higher r2 and lower rmse considers a good fit (going with model2)


#### method 2 stepwise selection through aic####

#1st model with only intercept
start <- lm(charges~1, data =insurance)

#2nd model with all predictor variables
all <- lm(charges~.,data = insurance)
formula(all)

#performing stepwise to get best model with lower aic
step(start, direction = "both", scope = formula(all))

#### appendix ####
#model comparison through anova and aic,bic

#h0: adding bmi, children and age does not improve the model
#h1: adding bmi, children and age does improve the model
#comparing models with anova
fit1 <- lm(formula = charges ~ smoker , data = insurance)
fit2 <- lm(formula = charges ~ smoker + bmi + children + age, data = insurance)
alpha = 0.05
ann <- anova(fit1,fit2)
p.value <- ann$`Pr(>F)`[2]
ifelse(p.value>alpha,"Fail to reject Null Hypothesis","Reject Null Hypothesis")

#using aic to compare
AIC(fit1,fit2)#low aic means best
BIC(fit1,fit2)#low bic means best
#fit2 is preferred.
