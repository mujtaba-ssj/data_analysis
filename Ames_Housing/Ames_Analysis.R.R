library(plyr)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(leaps)
library(car)
library(performance)


#1# Loading the dataset.
setwd("C:\\Users\\muj_m\\Desktop\\aly_6015")
ds <-  read.csv('AmesHousing.csv', header=TRUE)

#renaming variables correctly
colnames(ds)[1] <- 'Order'
colnames(ds)[25] <- "Exterior.1"
colnames(ds)[26] <- "Exterior.2"
colnames(ds)[45] <- "1st.Flr.SF"
colnames(ds)[46] <- "2nd.Flr.SF"
colnames(ds)[71] <- "3-Ssn.Porch"

#2# Exploratory data analysis for the data
str(ds)
summary(ds)

#getting all column names
colnames(ds)
dim(ds)

#no of house having ac
barplot(table(ds$Central.Air))

#no of house having good exposure
barplot(table(ds$Bsmt.Exposure))

#plotting sales price
ggplot(ds, aes(x = SalePrice)) + 
  geom_histogram(bins = 50, fill="gold",color="black")

ggplot(ds,aes(SalePrice, Overall.Qual , group=Overall.Qual, fill=Overall.Qual))+geom_boxplot() +labs(title = "Boxplot of Sale Price by Overall Quality of the houses", x="Sale
Price", y="Overall Quality") +theme_classic(base_size = 10)

#3# missing data being filled with mean
ds$Lot.Frontage[is.na(ds$Lot.Frontage)] <-
  mean(ds$Lot.Frontage, na.rm = T)

ds$Mas.Vnr.Area[is.na(ds$Mas.Vnr.Area)] <-
  mean(ds$Mas.Vnr.Area, na.rm = T)

ds$Garage.Yr.Blt[is.na(ds$Garage.Yr.Blt)] <-
  mean(ds$Garage.Yr.Blt, na.rm = T)

ds$Garage.Area[is.na(ds$Garage.Area) == T] <- mean(ds$Garage.Area,
                                                       na.rm=TRUE)

ds$Bsmt.Half.Bath[is.na(ds$Bsmt.Half.Bath) == T] <- mean(ds$Bsmt.Half.Bath,
                                                   na.rm=TRUE)

ds$Bsmt.Full.Bath[is.na(ds$Bsmt.Full.Bath) == T] <- mean(ds$Bsmt.Full.Bath,
                                                      na.rm=T)
ds$Garage.Cars[is.na(ds$Garage.Cars)] <-
  mean(ds$Garage.Cars, na.rm = T)

ds$Total.Bsmt.SF[is.na(ds$Total.Bsmt.SF)] <-
  mean(ds$Total.Bsmt.SF, na.rm = T)


#4#Correlation based on numerical variables.
num_cols <- sapply(ds, is.numeric)
data_num <- ds[,num_cols]

cors <- cor(data_num,use =
              "pairwise")
round(cors,2)

#5#Corr plot for all the numerical variables.
corrplot(cors, type = "upper", col =brewer.pal(n=10, name="RdYlBu"), tl.cex = 0.70, tl.col = 'black')

#6#plotting with high, low & 0.5 correlations

#high
plot(data_num$SalePrice, data_num$Gr.Liv.Area, xlab= "SalePrice", ylab= "Gr.Living Area")

#low
plot(data_num$SalePrice, data_num$BsmtFin.SF.2, xlab= "SalePrice", ylab= "BsmtFin.SF.2")

#0.5
plot(data_num$SalePrice, data_num$Mas.Vnr.Area, xlab= "SalePrice", ylab= "Mas.Vnr.Area")

#7# fitting our regression model
fit <- lm(formula = SalePrice ~Gr.Liv.Area + Total.Bsmt.SF +Garage.Area, data = data_num)
fit
summary(fit)

plot(fit)

#8# 
#creating formula for our model

#9# Different plots generated for our model
plot(fit)

#10# Checking for multicollinearity
#we use check_collinearity function to check for multicollinearity
mc <- check_collinearity(fit)
mc

#we plot it to see the vif value.
plot(mc)

#11# We check for outliers in our model.
#ar1
array1 <- boxplot.stats(data_num$Gr.Liv.Area)$out
paste0("The minimum value of the ouliers is ", min(array1), " The maximum
value of the outliers is ", max(array1))

ggplot(data_num, aes(y=Gr.Liv.Area)) + geom_boxplot(fill="#0c4c8a") +
  theme_minimal()

#arr2
array2 <- boxplot.stats(data_num$Total.Bsmt.SF)$out
paste0("The minimum value of the ouliers is at ", min(array2), " The maximum
value of the outliers is at ", max(array2))
ggplot(data_num, aes(y=Total.Bsmt.SF)) + geom_boxplot(fill="#0c4c8a") +
  theme_minimal()

#arr3
array3 <- boxplot.stats(data_num$Garage.Area)$out
paste0("The minimum value of the ouliers is at ", min(array3), " The maximum
value of the outliers is at ", max(array3))
ggplot(data_num, aes(y=Garage.Area)) + geom_boxplot(fill="#0c4c8a") +
  theme_minimal()

#arr4
array4 <- boxplot.stats(data_num$SalePrice)$out
paste0("The minimum value of the ouliers is at ", min(array4), " The maximum
value of the outliers is at ", max(array4))
ggplot(data_num, aes(y=SalePrice)) + geom_boxplot(fill="#0c4c8a") +
  theme_minimal()

#12#
#attempt to correct any changes

#13#We try to find the best regression model

#helps us find best subset regression model
#selecting required numerical columns
dn <- data_num[c(4,5,6,7,10,11,12,13,14,18,29,39)]
#we apply regsubsets which gives us best regression all-subset model
leapy <- regsubsets(SalePrice~., data = dn, nbest = 1, method = "exhaustive")
#we use subsets to show which combination is the best
subs <- subsets(leapy, statistic = "adjr2", ylim = c(0.60, 1), xlim = c(0, 4))


#14#
#creating linear model for our best subset.
modelx<- lm(SalePrice~Overall.Qual, data = data_num)
summary(modelx)

plot(SalePrice~Overall.Qual, data= data_num)
abline(lm(data_num$SalePrice~data_num$Overall.Qual))
