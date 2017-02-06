getwd()
mmx <- read.csv("mmx.csv")
summary(mmx)
colSums(is.na(mmx))
library(dplyr)
library(ggplot2)
names(mmx)
#Data Exploration 
p<- ggplot(mmx,aes(x=NewVolSales,y=Base.Price))
p+geom_point() #Price and sales are inversely proportional
#Sales Vs Radio
p1<-ggplot(mmx,aes(x=NewVolSales,y=Radio))
p1+geom_point()
#Sales Vs. Tv
p2<-ggplot(mmx,aes(x=NewVolSales,y=TV))
p2+geom_point()

boxplot(mmx$Base.Price)
boxplot(mmx$Radio)
boxplot(mmx$TV_adstock)


#Sales vs. Instore 
#Instore: % of total stores participating in In Store promotional activites each week 
p3<-ggplot(mmx,aes(x=NewVolSales,y=InStore))
p3+geom_point()

library(RColorBrewer)

#Sales vs.Stockout
p4<-ggplot(mmx,aes(x=NewVolSales,y=StockOut.,colour=Discount))
p4+geom_point()+scale_color_gradient(low="blue",high="red")

mmx$web <- ifelse(mmx$Website.Campaign=="Website Campaign",1,0)
mmx$Facebook <- ifelse(mmx$Website.Campaign=="Facebook",1,0)
mmx$Twitter <- ifelse(mmx$Website.Campaign=="Twitter",1,0)
mmx$Newspaper <- ifelse(mmx$NewspaperInserts=="Insert",1,0)
mmx$online<- mmx$Facebook+mmx$Twitter+mmx$web
mmx<- mmx[c(-6,-7)]
# Adstock effect
Decay=0.7 #For TV 
mmx$TV_adstock <- stats::filter(mmx$TV,filter=Decay,method="recursive")
mmx$Radio_adstock <- stats::filter(mmx$Radio,filter=0.5,method="recursive")
mmx$Newspaper_adstock <- stats::filter(mmx$Newspaper,filter=0.3,method="recursive")
mmx$online_adstock <- stats::filter(mmx$online,filter=0.3,method="recursive")
summary(mmx)

#Sampling data
set.seed(123)
sampling<-sort(sample(nrow(mmx), nrow(mmx)*.7)) 
length(sampling)

#Select training sample
train<-mmx[sampling,]
test<-mmx[-sampling,]

# Linear Regression

library(MASS)
reg<- lm (NewVolSales ~ Base.Price + InStore + StockOut. + Discount + TV_adstock + 
            online_adstock + Newspaper_adstock,data=train)
summary(reg)

#Removing insignificant predictor variables

reg1 <- lm(NewVolSales ~ Base.Price+InStore+StockOut.+TV_adstock,data=train)
summary(reg1)


##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity

pred_train <- predict(reg1)
Res_train <- resid(reg1)

plot(pred_train,Res_train,abline(0,0))
plot(train$NewVolSales,Res_train,abline(0,0))
#Since in both cases there is no pattern, assumptions are satisfied

##Checking Multi-Collinearity
library(car)
vif(reg1)

##Plotting actual vs predicted values
plot(train$NewVolSales,col="blue",type="l")
lines(pred_train,col="red",type="l")


par(mfrow=c(2,2)) # partition the graphics device
plot(reg1)

# Test dataset
pred <- predict(reg1,data=test)

##Plotting actual vs predicted values
plot(test$NewVolSales,col="blue",type="l")
lines(pred,col="red",type="l")


