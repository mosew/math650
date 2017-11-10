############## HW 5 ###############

library(MASS)
library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(ggplot2)

#8
#a
set.seed(438)
train = sample(1:nrow(Carseats),0.7*nrow(Carseats))

#b
carseats.tree=tree(Sales~.,data=Carseats,subset=train)
plot(carseats.tree)
text(carseats.tree,pretty=0)

yhat=predict(carseats.tree,newdata=Carseats[-train,])
mean(yhat-Carseats$Sales[-train])
# [1] 0.3660649

#c
set.seed(4234)
cv.carseats=cv.tree(carseats.tree)
# The tree of size 9, complexity parameter 41.08212 is optimal. Thus pruning the tree is a good idea.

plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats = prune.tree(carseats.tree, best=9)

#d
set.seed(3)
bag.carseats=randomForest(Sales~.,data=Carseats,mtry=10,subset=train,importance=T)
mean(predict(bag.carseats,newdata=Carseats[-train,])-Carseats$Sales[-train])
# [1] 0.3793636
importance(bag.carseats)
#                 %IncMSE IncNodePurity
# CompPrice   32.2439725    226.395638
# Income      10.4108335    119.671656
# Advertising 23.3176622    166.001802
# Population  -1.3676633     68.047007
# Price       69.7143167    651.816072
# ShelveLoc   67.2617087    643.843985
# Age         17.1493934    184.297243
# Education    0.3547737     60.571779
# Urban       -2.1662896      6.842325
# US           1.0786678      8.549974

#e
set.seed(334)
mse=rep(0,9)
for(i in 1:9){
  rf.carseats=randomForest(Sales~.,data=Carseats,mtry=i,subset=train,importance=T)
  mse[i]=mean((predict(rf.carseats,newdata=Carseats[-train,])-Carseats$Sales[-train])^2)
}

which(mse==min(mse))
# [1] 9

plot(1:9,mse)
# Looks like test error decreases almost monotonically as mtry increases, which I think is supposed to be unusual.

importance(rf.carseats)
#                 %IncMSE IncNodePurity
# CompPrice   30.165926    225.400070
# Income      11.893220    123.665263
# Advertising 24.047374    167.867271
# Population  -1.929543     73.672692
# Price       70.214615    632.309221
# ShelveLoc   67.435373    640.226555
# Age         18.050489    187.451379
# Education    1.403993     59.931463
# Urban       -1.937819      7.287750
# US           1.003356      9.185257


#10
#a
h=Hitters[!is.na(Hitters$Salary),]
h$Salary=log(h$Salary)

#b
training=1:200

#c
set.seed(323)
test.mse.h=train.mse.h=rep(0,21)
for(j in 0:20){
  i=10^(j/10-3) #ranges from 10^-3 to 10^-1
  boost.h=gbm(Salary~.,data=h[training,],n.trees=1000,distribution="gaussian",shrinkage=i)
  train.mse.h[j+1]=mean(boost.h$train.error^2)
  test.mse.h[j+1]=mean((predict(boost.h,newdata=h[-training,],n.trees=1000)-h$Salary[-training])^2)
}

x=10^seq(-3,-1,.1)
y.train=train.mse.h
y.test=test.mse.h
min(y.test)
#[1] 0.2528007

z=data.frame(x,y.train,y.test)
ggplot(z,aes(x=x,y=y.train))+geom_point()

#d
ggplot(z,aes(x=x,y=y.test))+geom_point()

#e
lm.h=lm(Salary~.,data=h[training,])
lm.test.mse.h=mean((predict(lm.h,newdata=h[-training,])-h$Salary[-training])^2)
lm.test.mse.h
#[1] 0.4917959

rf.h=randomForest(Salary~.,data=h[training,])
rf.test.mse.h=mean((predict(rf.h,newdata=h[-training,])-h$Salary[-training])^2)
#[1] 0.2120292

#f
summary(boost.h)
# var                   rel.inf

# CAtBat       CAtBat 20.4336070
# CRBI           CRBI  8.0107166
# PutOuts     PutOuts  7.8324233
# CWalks       CWalks  7.3856510

# CHmRun       CHmRun  5.8049041
# Walks         Walks  5.7749994
# Hits           Hits  5.5911013
# CRuns         CRuns  5.5092282
# Years         Years  5.4611522
# Assists     Assists  5.4512058
# CHits         CHits  4.7812253
# AtBat         AtBat  4.1435867
# RBI             RBI  3.7323176
# HmRun         HmRun  3.6619915
# Runs           Runs  2.6841270
# Errors       Errors  2.2478086
# NewLeague NewLeague  0.6556320
# Division   Division  0.5836653
# League       League  0.2546573

#g
bag.h=randomForest(Salary~.,data=h[training,],mtry=7,ntree=25)
mean((predict(bag.h,newdata=h[-training,])-h$Salary[-training])^2)
# [1] 0.2207391


#11
#a
training=1:1000

#b
set.seed(432)
Caravan$Purchase=as.integer(Caravan$Purchase)-1 #To scale to 0-1.
boost.c=gbm(Purchase~.,data=Caravan[training,],n.tree=1000,distribution="bernoulli",shrinkage=.01,verbose=F)
summary(boost.c)
#              var     rel.inf
# PPERSAUT PPERSAUT 14.65556383
# MKOOPKLA MKOOPKLA  9.74839024
# MOPLHOOG MOPLHOOG  6.89067168
# MBERMIDD MBERMIDD  6.13449305
# PBRAND     PBRAND  5.20530118
# ABRAND     ABRAND  4.52397128
# MGODGE     MGODGE  4.35190008
# MINK3045 MINK3045  3.82683945
# PWAPART   PWAPART  2.80348805
# ...

#c
pred.c=predict(boost.c,newdata=Caravan[-training,-86],n.trees=1000)
# I have no idea what's going on here. The values are all negative. I've investigated for a very long time and give up.

glm.c=glm(Purchase~.,data=Caravan[training,])
glm.pred=predict(glm.c,newdata=Caravan[-training,])
table(glm.pred>0.2,Caravan$Purchase[-training])
mean(as.numeric((glm.pred>0.2)==Caravan$Purchase[-training]))
