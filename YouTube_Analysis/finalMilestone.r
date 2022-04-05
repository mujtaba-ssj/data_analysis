####Milestone 1 ####

#first 40 lines are required for all the milestones to be accessed
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(purrrlyr)
library(corrplot)
library(ggpubr)

#loading data
setwd("C:\\Users\\muj_m\\Desktop\\Aly_6010\\project")

#Since data has special characters, so making use of enconding
df <- read.csv("CAvideos.csv", encoding = "UTF-8")

head(df)
str(df)
sum(is.na(df))
summary(df)

colnames(df)
#data cleaning (removing non usable variables)
# and further cleaning with names and different datatype
colnames(df)[11]="comments"
df$comments_disabled <- as.logical(df$comments_disabled)
df$ratings_disabled <- as.logical(df$ratings_disabled)
df$video_error_or_removed <- as.logical(df$video_error_or_removed)

df$tags<-NULL
df$thumbnail_link<-NULL
df$description<-NULL

#working with time and getting them in correct format
df$trending_date<- format(as.Date(df$trending_date, '%y.%d.%m'), "%Y/%m/%d")
df$trending_date <-as.Date(df$trending_date, "%Y/%m/%d")
df$publish_time <- as.Date(df$publish_time, "%Y-%m-%d")



#
str(df)
dim(df)
nrow(df)
ncol(df)
colnames(df)


#overall correlation
ggplot(df)+geom_point(mapping=aes(x=views,y=likes))

#subset of a channel
Em <- subset(df, channel_title=="EminemVEVO" )

#eminem songs
ggplot(data=Em)+
  geom_point(mapping=aes(x = views, y = likes, color = title))

#getting latest rows for each
lat<-df %>% 
  group_by(title) %>%
  slice(which.max(as.Date(trending_date, '%y/%m/%d')))

latt <- data.frame(lat)

#categories within youtube dataset
unique(df$category_id)

#frequency table for category
fqt <- table(df$category_id)
View(fqt)
barplot(fqt, ylim = c(0,14000), xlab = "Category ID", ylab = "Frequency", main="Number of videos for each category", col="#52BE80", las=2)

#getting the ones with most views
mv <- data.frame(lat[order(lat$views, decreasing = TRUE),])

#top 10 views
mvh <- head(mv, 10)

#plot for top views
plmv <- ggplot(data = mvh)+geom_point(mapping = aes(x=trending_date, y=views, text=views,color = title))
ggplotly(plmv, tooltip = "text")


#getting the ones with most likes
ml <- lat[order(lat$likes, decreasing = TRUE),]
head(ml)

#top 10 likes
mln <- head(ml,10)

ggplot(data = mln)+geom_point(mapping = aes(x=title, y=views,color = likes))+theme(axis.text.x = element_text(angle=90, vjust=0.5))


#getting the ones with most dislikes
mdl <- lat[order(lat$dislikes, decreasing = TRUE),]

mdlh <- head(mdl, 10)

#vid dislikes
ggplot(mdlh, aes(x=channel_title, y=dislikes)) + 
  geom_bar(stat="identity", width=.5, fill="#FA8072") + 
  labs(title=" Top 10 Channels with most dislikes", 
       subtitle="Title Vs Dislikes", 
       caption="source: mdlh") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6, size = 7))


#videos with no likes and rating disabled
nl <- lat[lat$likes==0,]
nl <- nl[order(nl$views,decreasing = TRUE),]
nlh = head(nl,10)

pie(nlh$views,nlh$channel_title,col = c("#DE3163","#FF7F50","#FFBF00","#DFFF00","#CCCCFF","#A569BD","#45B39D","#9FE2BF","#40E0D0","#6495ED"), main = "Top 10 Channels with no likes")

#video with comments disabled
cd <- df[df$comments_disabled==TRUE,]
cdd <- table(cd$comments)
View(cdd)

#checking for videos with more than a million views
mvtable <- table(df$views>1000000)
View(mvtable)

#checking for videos with more than a 100k likes
mltable <- table(df$likes>100000)
View(mltable)

#checking for videos with more than a 100k dislikes
mdltable <- table(df$dislikes>100000)
View(mdltable)

#plotting with four variables
topp <- head(lat,15)
top <- topp[order(topp$views,decreasing = TRUE),]
plmv <- ggplot(data = top)+geom_point(mapping = aes(x=channel_title, y=views, text=dislikes ,color = likes))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplotly(plmv, tooltip = "text")

#### Milestone 2 ####

#Q1#
#a.	Which category of videos has the highest number of total views? 
#A company wants to place their entertainment ad on either music or entertainment 
#video who has good reach but is confused which one would benefit him.
#to work on this we'll select a sample from the general population.

#we try to find the category with most overall views
group5 <- data.frame(Subject=df$video_id, pt=df$views, category= df$category_id)

ci <-group5 %>%
  group_by(category) %>%
  summarize(views = sum(pt))

ci <- ci[order(ci$views, decreasing = TRUE),]
View(ci)
#from the above we find cat 10 & 24 has most views

#Arranging the data
lat<-df %>% 
  group_by(title) %>%
  slice(which.max(as.Date(trending_date, '%y/%m/%d')))
lat <- lat[order(lat$trending_date, decreasing = FALSE),]


#subset of 10 and 24
#we see that cat 24 has more number of videos that cat10, but is the reach the same?
cat10 <- subset(lat,category_id==10)
cat24 <- subset(lat,category_id==24)

sum(cat10$views)
sum(cat24$views)
blog1 <- subset(lat,category_id==10|category_id==24)

#Number of records each category has
blog1$category_id <- as.factor(blog1$category_id)

blog1$category_id

we <- table(blog1$category_id)
barplot(we, xlab="Category ID", ylab="No of records", main="Records for both the categories", ylim = c(0,9000), col = "pink")

#boxplot for category
ggplot(blog1, aes(x=category_id, y=views)) + 
  geom_boxplot()

#hypothesis
#test for cat10
shapiro.test(cat10$views)

#performing t test
t.test(cat10$views,cat24$views)

##random question#
#going for timebased
#youtube wants to know the time based views for both the categories
ct2 <- cat24[order(cat24$views, decreasing = TRUE),]
ct1 <- cat10[order(cat10$views, decreasing = FALSE),]

#creating time based graph for highest views in cat 24
ct2 %>% 
  ggplot( aes(x=trending_date, y=views)) +
  geom_line(color="#69b3a2") +
  ylim(0,138000000) +
  annotate(geom="text", x=as.Date("2017-12-13"), y=137943120, 
           label="Highest views was around 137 million") +
  annotate(geom="point", x=as.Date("2017-12-13"), y=137843120, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=50000000, color="orange", size=.5) +
  theme_ipsum()

#creating time based graph for highest views in cat 10
ct1 %>% 
  ggplot( aes(x=trending_date, y=views)) +
  geom_line(color="#69b3a2") +
  ylim(0,100000000) +
  annotate(geom="text", x=as.Date("2018-05-18"), y=98938809, 
           label="Highest views was around 980 million") +
  annotate(geom="point", x=as.Date("2018-05-15"), y=98938809, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=48000000, color="orange", size=.5) +
  theme_ipsum()

#performing two-sample t-test
t.test(ct2$views,ct1$views)

#Q2#
#which is the most disliked video ? As a company would select the most liked, 
#they would like to know disliked as well so as to stay away from it.

dislike <- df[(df$dislikes),]

#checking for disliked videos
dislike <- dislike[order(dislike$dislikes, decreasing = TRUE),]

#specifying the data 
group <- data.frame(Subject=dislike$video_id, pt=dislike$dislikes, title= dislike$title)

#extracting latest rows based on video title
zz <-group %>%
  group_by(title) %>%
  summarize(dislikes = max(pt))

#arranging in descending order
dis <- zz[order(zz$dislikes, decreasing = TRUE),]

#top 10 overall most dislike in 17 & 18
d1 <- head(dis,10)

#pie chart with top 10 disliked vids
ggplot(d1, aes(x = "", y = dislikes, fill = title)) +
  geom_col(color = "black") +
  geom_text(aes(label = dislikes),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

#performing one sample t test
t.test(dis$dislikes, mu= 2000, alternative = "less")

#liked videos
group1 <- data.frame(Subject=df$video_id, pt=df$likes, title= df$title)

#extracting latest rows based on video title
zaz <-group1 %>%
  group_by(title) %>%
  summarize(likes = max(pt))

#arranging them in descending order
lik <- zaz[order(zaz$likes, decreasing = TRUE),]

#top 10 with most likes
df10 <- head(lik,10)

#piechart for the likes
ggplot(df10, aes(x = "", y = likes, fill = title)) +
  geom_col(color = "black") +
  geom_text(aes(label = likes),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

#t test for likes
t.test(df$likes, mu = 35000, alternative = "less")

#Q3#
#videos where crowd was engaging (more comments)Youtube wants to know which video 
#has more comments to put up survey
#find max in comments and compared between top 2
#comments
#comments closed person wants to know which video has their comments closed
# and does it affect the views 
cx <- df[(df$comments_disabled==TRUE),]
group3 <- data.frame(Subject=cx$video_id, pt=cx$views, title= cx$channel_title)

#getting latest rows based on channel title
cx1 <-group3 %>%
  group_by(title) %>%
  summarize(views = max(pt))
#arranging them in descending order
cx2 <- cx1[order(cx1$views, decreasing = TRUE),]

#getting top 10 rows with most views
cx2_10 <- head(cx2,10)

ggplot(cx2_10, aes(x = "", y = views, fill = title)) +
  geom_col(color = "black") +
  geom_text(aes(label = views),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")

#shapiro 2 for normality
shapiro.test(cx$likes)
#we run t test to see whether there is a similarity between the views and dislikes
#since the comments are closed.
t.test(cx$views,cx$likes)
#we see even though they have comments blocked some of the videos have over 100
#million views while other has around 500 mil

####Milestone 3####

#2018 first five months
dd <- df[df$trending_date >= as.Date("180101", "%y%m%d") & df$trending_date <= as.Date("180531", "%y%m%d"), ]

#category 10 data
cat10 <- subset(dd, category_id == 10)

#category 10 data in descending order
cat10d <- cat10[order(cat10$likes, decreasing = TRUE),]


#extracting latest rows based on video title
categ10<-cat10d %>% slice_rows("title") %>% dmap(max)

#showcasing needed data
categ10$video_id <- NULL
categ10$publish_time <- NULL
categ10$ratings_disabled <- NULL
categ10$video_error_or_removed <- NULL
categ10$comments_disabled <- NULL
categ10$trending_date <- NULL

#descriptive statistics of views and likes
summary(categ10$views)
summary(categ10$likes)
#regression plot
plot(categ10$views,categ10$likes,col="green", xlab = "Views", ylab="Likes")
abline(lm(data=categ10, likes~views))
text(paste("Correlation:", round(cor(categ10$views, categ10$likes), 2)), x = 4e+07, y = 4e+06)

#linear regression equation
lrvl <- lm(data=categ10, likes~views)
summary(lrvl)

#finding correlation 
views<- categ10$views
likes <- categ10$likes
vl <- data.frame(views,likes)
vl <- round(cor(vl),2) 
vl
#finding correlation plot
corrplot(vl,method = "pie")


#Q. youtube says that if you were to upload videos under this cat10 then you would
#get more than million views

#hypothesis test
summary(categ10$views)
t.test(categ10$likes, mu = 40000, alternative = "greater")
lml <- lm(data=categ10, likes~views)
summary(lml)


#Q. correlation between views and dislikes (dependent is dislikes, independent is views)
dis <- categ10[order(categ10$dislikes, decreasing = TRUE),]


#dislikes statistics
summary(categ10$views)
summary(categ10$dislikes)

#hypo test one sample
t.test(categ10$dislikes, mu=5000, alternative="greater")

#difference in likes and difference, two sample test
t.test(categ10$likes,categ10$dislikes)

#regression plot
plot(dis$views,dis$dislikes,col="maroon", xlab="views", ylab="dislikes")
abline(lm(data = dis,dislikes~views))
text(paste("Correlation:", round(cor(categ10$views, categ10$dislikes), 2)), x = 2e+07, y = 150000)

#finding correlation
views<- categ10$views
dislikes <- categ10$dislikes
vd <- data.frame(views,dislikes)
vd <- round(cor(vd),2) 
vd
#correlation plot
corrplot(vd,method = "pie")

#correlation coefficient
lindis <- lm(data = categ10,dislikes~views)
summary(lindis) #0.0016, -390
