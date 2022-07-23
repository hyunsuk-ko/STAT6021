## store data file with the variable name Data
Data<-read.csv("rocket.csv", header=TRUE)
head(Data)

##remove first column
Data<-Data[,-1]
##rename the remaining 2 columns
names(Data)<-c("Strength", "Age")
head(Data)

##Fit a regression model
result<-lm(Strength~Age, data=Data)
summary(result)

##to produce 95% CIs for all regression coefficients
confint(result,level = 0.95)

##to produce 95% CI for the mean response when x=10, 
##and the 95% PI for the response of an observation when x=10
newdata<-data.frame(Age=10)
predict(result,newdata,level=0.95, interval="confidence")
predict(result,newdata,level=0.95, interval="prediction")

###############################
##extra stuff not in tutorial##
###############################

##overlay prediction interval on scatterplot

library(tidyverse)

##regular scatterplot
ggplot(Data, aes(x=Age, y=Strength))+
  geom_point() +
  labs(x="Age (Weeks)", 
       y="Strength (PSI)", 
       title="Scatterplot of Bond Strength vs Age")

##with regression line overlaid, and bounds of CI for mean y
ggplot(Data, aes(x=Age, y=Strength))+
  geom_point() +
  geom_smooth(method=lm)+
  labs(x="Age (Weeks)", 
       y="Strength (PSI)", 
       title="Scatterplot of Bond Strength vs Age")

##only regression line overlaid
ggplot(Data, aes(x=Age, y=Strength))+
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  labs(x="Age (Weeks)", 
       y="Strength (PSI)", 
       title="Scatterplot of Bond Strength vs Age")

##overlay PIs
##find PIs for each observation
preds <- predict(result, interval="prediction")

Data<-data.frame(Data,preds)

ggplot(Data, aes(x=Age, y=Strength))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)+
  labs(x="Age (Weeks)", 
       y="Strength (PSI)", 
       title="Scatterplot of Bond Strength vs Age")
