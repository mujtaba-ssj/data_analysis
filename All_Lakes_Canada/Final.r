#importing libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(hrbrthemes)


#loading data
setwd("C:\\Users\\muj_m\\Desktop\\Aly_6010")
stats <- read.csv("All_Lakes_GLIP.csv")
stat <- stats
str(stat)
#get all the variables
colnames(stats)

#renaming column
colnames(stats)[4]="DATE"
colnames(stats)[2]="FACILITY"
colnames(stats)[13]="QUALIFIER"
colnames(stats)[14]="DESCRIPTION"
colnames(stats)

#Converting date from factor to DATE format
stats$DATE <- as.Date(stats$DATE, "%m/%d/%Y")
str(stats)

#Unique values in the data
unique(stats$LAKE)
unique(stats$FACILITY)
unique(stats$YEAR)
unique(stats$TEST_CODE)
unique(stats$PARAMETER)
unique(stats$UNIT)

#substituting empty rows with "empty" string with gsub
stats$DESCRIPTION <- gsub("^$|^ $","Empty",stats$DESCRIPTION)

#dropping column
stats$ANALYTIC_METHOD<-NULL

#Descriptive Statistics
class(stats)
nrow(stats)
ncol(stats)
head(stats)
tail(stats)
str(stats)
summary(stats)

#total rows and columns
dim(stats)

#total empty rows
sum(is.na(stats))

#Lakes and facility freq
tt <- rename(count(stats, LAKE, FACILITY), Freq = n)
view(tt)

#Cross tabulation with lake and facility
crossy <- xtabs(~ LAKE+FACILITY,data=stats)
View(crossy)

ggplot(data = tt)+geom_point(mapping = aes(x=LAKE, y=Freq,color = FACILITY))


#Lakes and parameter freq
lp <- rename(count(stats, LAKE, PARAMETER), Freq = n)
View(lp)

#chemicals in lake
content<-xtabs(~LAKE+PARAMETER,data=stats)
View(content)

#ggplotly for lakes and parameter with freq
pip <- ggplot(data = lp)+geom_point(mapping = aes(x=LAKE, y=Freq,color = PARAMETER))
ggplotly(pip)

#ggplot with lakes year content and freq
yp <- rename(count(stats, LAKE, PARAMETER, YEAR), Freq = n)
View(yp)

lyc <- ggplot(data = yp)+geom_point(mapping = aes(x=LAKE, y=Freq, text=YEAR,color = PARAMETER))
ggplotly(lyc, tooltip = "text")


#no of records for each lake
lakecount <- table(stats$LAKE)
lakecount

#no of record for each facility
fcc <- table(stats$FACILITY)
View(fcc)

#no of records for parameters
paracount <- table(stats$PARAMETER)
View(paracount)

#plot
barplot(lakecount,col="purple", ylim=c(0, 170000), xlab = "Lakes", ylab="Frequency")

#facility freq plot
par(mar=c(10,4,4,2))
barplot(fcc,col="lightgreen", ylim=c(0, 30000), xlab = "Facilities", ylab="Frequency",las=3,cex.names=.5)  

#parameter plot
barplot(paracount,las=2, xlab = "Parameters", ylab="Frequency", cex.names=.75, col="#a9dfbf")


#cross tab with lake facility and their parameters
pol <- xtabs(~ LAKE+FACILITY+PARAMETER,data=stats)
View(pol)

#Getting rows based on chloride
cl <- filter(stats, PARAMETER=="Chloride")
cl <- cl[order(cl$VALUE,decreasing = TRUE),]

summary(cl)

#overall high chloride range
cl %>% 
  ggplot( aes(x=DATE, y=VALUE)) +
  geom_line(color="#69b3a2") +
  ylim(0,700) +
  annotate(geom="text", x=as.Date("1999-01-17"), y=650, 
           label=" highest Chloride level") +
  annotate(geom="point", x=as.Date("1999-08-17"), y=607, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=200, color="orange", size=.5) +
  theme_ipsum()

#top 10 records with most chloride
barplot(head(cl$VALUE,n=10),names.arg = head(cl$LAKE,n=10),las=2,cex.names = 0.8, col="#d2b4de", xlab = "Lake",ylab = "Value",main ="Records with most chloride content")
hist(cl$YEAR, col = "#48c9b0", main = "Histogram for CL records over the years", xlab="Years")
max(cl$VALUE)
min(cl$VALUE)

#chloride for each lake
ls = cl[cl$LAKE=="Lake Superior",]
le = cl[cl$LAKE=="Lake Erie",]
lh = cl[cl$LAKE=="Lake Huron",]
lo = cl[cl$LAKE=="Lake Ontario",]

#Chloride level in lake superior
th<- ls %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("Chloride level in Lake Superior")+
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Chloride level") +
  theme_ipsum()
ggplotly(th)

#chloride level in lake erie
te<- le %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("Chloride level in Lake Erie")+
  geom_area(fill="#c39bd3", alpha=0.5) +
  geom_line(color="#c39bd3") +
  ylab("Chloride level") +
  theme_ipsum()
ggplotly(te)

#lake huron chloride
th<- lh %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("Chloride level in Lake Huron")+
  geom_area(fill="#f5cba7", alpha=0.5) +
  geom_line(color="#f5cba7") +
  ylab("Chloride level") +
  theme_ipsum()
ggplotly(th)

#lake ontario chloride
to<- lo %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("Chloride level in Lake Ontario")+
  geom_area(fill="#a9dfbf", alpha=0.5) +
  geom_line(color="#a9dfbf") +
  ylab("Chloride level") +
  theme_ipsum()
ggplotly(to)


#nitrogen for each lake

#nitrogen nitrate
nitra <- filter(stats, PARAMETER=="Nitrogen; nitrate")

nit <- nitra[order(nitra$VALUE,decreasing = TRUE),]

nit %>% 
  ggplot( aes(x=DATE, y=VALUE)) +
  geom_line(color="#f5b7b1") +
  ylim(0,6) +
  annotate(geom="text", x=as.Date("1981-02-23"), y=6, 
           label=" highest nitrate level") +
  annotate(geom="point", x=as.Date("1981-02-23"), y=5.50, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=2, color="orange", size=.5) +
  theme_ipsum()


#top 10 records with most nitrate
barplot(head(nit$VALUE,n=10),names.arg = head(nit$LAKE,n=10),las=2,cex.names = 0.8, col="#d2b4de", xlab = "Lake",ylab = "Value(mg/L)",main ="Records with most Nitrate content")


#nitrate for each lake
ns = nit[nit$LAKE=="Lake Superior",]
ne = nit[nit$LAKE=="Lake Erie",]
nh = nit[nit$LAKE=="Lake Huron",]
no = nit[nit$LAKE=="Lake Ontario",]

#nitrate level in lake superior
tn<- ns %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("nitrate level in Lake Superior")+
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("nitrate level") +
  theme_ipsum()
ggplotly(th)

#nitrate level in lake erie
tne<- ne %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("nitrate level in Lake Erie")+
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("nitrate level") +
  theme_ipsum()
ggplotly(tne)

#lake huron nitrate
tnh<- nh %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("nitrate level in Lake Huron")+
  geom_area(fill="#f5cba7", alpha=0.5) +
  geom_line(color="#f5cba7") +
  ylab("nitrate level") +
  theme_ipsum()
ggplotly(tnh)

#lake ontario nitrate
tno<- no %>%
  ggplot( aes(x=DATE, y=VALUE)) + 
  ggtitle("nitrate level in Lake Ontario")+
  geom_area(fill="#a9dfbf", alpha=0.5) +
  geom_line(color="#a9dfbf") +
  ylab("nitrate level") +
  theme_ipsum()
ggplotly(tno)

#nitrogen nitrite
nitri <- filter(stats, PARAMETER=="Nitrogen; nitrite")

#nitrite
nitr <- nitri[order(nitri$VALUE,decreasing = TRUE),]
nitr %>% 
  ggplot( aes(x=DATE, y=VALUE)) +
  geom_line(color="#f5b7b1") +
  ylim(0,1) +
  annotate(geom="text", x=as.Date("1991-10-01"), y=0.62, 
           label=" highest nitrite level") +
  annotate(geom="point", x=as.Date("1991-10-01"), y=0.62, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=2, color="orange", size=.5) +
  theme_ipsum()

