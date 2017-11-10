#HW 1
#Mose Wintner

library(ISLR)

#assignment 8
#a)
college<-read.csv("College.csv")
rownames(college)=college[,1]

#b)
college=college[,-1]

#c)
#i
summary(college)
#ii
pairs(college[,1:10])
#iii
boxplot(college$Outstate~as.factor(college$Private))
#iv
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)
summary(college)
#v
college=data.frame(college,N.Undergrad=college$F.Undergrad + college$P.Undergrad)
Big=rep("No",nrow(college))
Big[college$N.Undergrad>9000]="Yes"
Big=as.factor(Big)
college=data.frame(college,Big)

AvgCost=rep(0,nrow(college))
AvgCost=college$Personal+college$Books+college$Room.Board+college$Expend
college=data.frame(college,AvgCost)

Expensive=rep("No",nrow(college))
Expensive[college$AvgCost>20000]="Yes"
Expensive=as.factor(Expensive)
college=data.frame(college,Expensive)

#9
#a)
Auto=read.csv("Auto.csv", header=T,na.strings="?")
AutoNumeric<-Auto
AutoNumeric$origin<-NULL
AutoNumeric$name<-NULL
#b) and c)
for (i in names(AutoNumeric)){
  x<-AutoNumeric[[i]]
  print(i)
  print(range(x, na.rm=TRUE))
  print(mean(x,na.rm=TRUE))
  print(sd(x,na.rm=T))
}
#d)
AutoNumeric<-AutoNumeric[-10:-85]
for (i in names(AutoNumeric)){
  x<-AutoNumeric[[i]]
  print(i)
  print(range(x, na.rm=TRUE))
  print(mean(x,na.rm=TRUE))
  print(sd(x,na.rm=T))
}

#e)
pairs(data.frame(
  Auto$mpg,
  as.factor(Auto$cylinders),
  Auto$horsepower,
  Auto$displacement,
  Auto$weight
  ))

#10
library(MASS)
summary(Boston)
pairs(Boston[c(3,5,7,8,10)])
pairs(Boston[c(6,13,14)])
#c)
cor(Boston,Boston$crim)
#e)
sum(Boston$chas==1)
#f)
median(Boston$ptratio)
#g)
Boston[399,]
#h)
sum(Boston$rm>=7)
sum(Boston$rm>=8)
Boston[Boston$rm>=8,]
