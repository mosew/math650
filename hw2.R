library(ISLR)

#10)
w=Weekly

#a)
pairs(w)
plot(w$Volume~w$Year)
cor(w[,-9])

#b)
glm.fit=glm(Direction~Volume+Lag1+Lag2+Lag3+Lag4+Lag5,data=w,family=binomial)
summary(glm.fit)
summary(glm.fit$coefficients)

#c)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,w$Direction)
mean(glm.pred==w$Direction)

#d)
b= w$Year>=1990 & w$Year<=2008

glm.fit2=glm(Direction~Lag2,data=w,family=binomial)
glm.probs2=predict(glm.fit2,type="response")
glm.pred2=rep("Down",1089)
glm.pred2[glm.probs2>.5]="Up"
table(glm.pred2[!b],w$Direction[!b])
mean(glm.pred2[!b]==w$Direction[!b])

#e)
library(MASS)
lda.fit=lda(Direction~Lag2, data=w, subset=b)
lda.pred=predict(lda.fit, w[!b,])
lda.class=lda.pred$class
table(lda.class,w$Direction[!b])
mean(lda.class==w$Direction[!b])

#f)
qda.fit=qda(Direction~Lag2, data=w, subset=b)
qda.pred=predict(qda.fit, w[!b,])
qda.class=qda.pred$class
table(qda.class,w$Direction[!b])
mean(qda.class==w$Direction[!b])

#g)
library(class)
train=cbind(w$Lag2[b])
test=cbind(w$Lag2[!b])
Direction=w$Direction[b]
set.seed(2)
knn.pred=knn(train,test,Direction,k=1)
table(knn.pred,w$Direction[!b])
mean(knn.pred==w$Direction[!b])

#h)
#-----------
knn_accuracy_by_k=sapply(1:30,function(i) mean(knn(train,test,Direction,k=i)==w$Direction[!b]))
plot(knn_accuracy_by_k,xlab="K",ylab="accuracy")
#------------

#On this part I just altered the covariates each time I ran a different analysis

boolDirection=rep(1,1089)
boolDirection[w$Direction=="Down"]=0

w$Lag2Direction=c(0,0,sapply(3:1089,function(k) w$Lag2[k]*boolDirection[k-2]))
w$VolumeLag2=w$Volume*w$Lag2


glm.fit3=glm(Direction~Lag2+VolumeLag2,data=w,family=binomial)
glm.probs3=predict(glm.fit3,type="response")
glm.pred3=rep("Down",1089)
glm.pred3[glm.probs3>.5]="Up"
table(glm.pred3[!b],w$Direction[!b])
mean(glm.pred3[!b]==w$Direction[!b])

lda.fit2=lda(Direction~Lag2+VolumeLag2, data=w, subset=b)
lda.pred2=predict(lda.fit2, w[!b,])
lda.class2=lda.pred2$class
table(lda.class2,w$Direction[!b])
mean(lda.class2==w$Direction[!b])

qda.fit2=qda(Direction~Lag2+VolumeLag2, data=w, subset=b)
qda.pred2=predict(qda.fit2, w[!b,])
qda.class2=qda.pred2$class
table(qda.class2,w$Direction[!b])
mean(qda.class2==w$Direction[!b])
#--------

#11)
#a)
a=Auto
a$mpg01n=as.numeric(a$mpg>median(a$mpg))
a$mpg01=as.factor(a$mpg>median(a$mpg))
#b)
par(mfrow=c(3,2))
plot(a$mpg01~a$cylinders)
plot(a$mpg01~a$displacement)
plot(a$mpg01~a$horsepower)
plot(a$mpg01~a$weight)
plot(a$mpg01~a$acceleration)
plot(a$mpg01~a$year)

#c)
set.seed(2)
#this picks about 70% of the data as training data
train=runif(392)>.3

#d)
lda.fitmpg=lda(mpg01~cylinders+horsepower+weight,data=a,subset=train)
lda.predmpg=predict(lda.fitmpg, a[!train,])
mean(lda.predmpg$class==a[!train,]$mpg01)

#e) 
qda.fitmpg=qda(mpg01~cylinders+horsepower+weight,data=a,subset=train)
qda.predmpg=predict(qda.fitmpg, a[!train,])
mean(qda.predmpg$class==a[!train,]$mpg01)

#f)
glm.fitmpg=glm(mpg01~cylinders+horsepower+weight,data=a,family=binomial)
glm.probsmpg=predict(glm.fitmpg,type="response")
glm.predmpg=round(glm.probsmpg)
mean(glm.predmpg[!train]==a[!train,]$mpg01n)

#g)
full=cbind(a$cylinders,a$horsepower,a$weight)
training=cbind(a$cylinders[train],a$horsepower[train],a$weight[train])
test=cbind(a$cylinders[!train],a$horsepower[!train],a$weight[!train])
trainingout=a$mpg01[train]
set.seed(2)
par(mfrow=c(1,1))
knn_accuracy_by_k=sapply(1:21, function(i) mean(knn(training,test,trainingout,k=i)==a$mpg01[!train]))
plot(knn_accuracy_by_k,xlab="K",ylab="accuracy")

#12)
#a)
Power = function(){print(2^3)}
Power()

#b)
Power2 = function(x,a){print(x^a)}

#c)
Power2(10,3)
Power2(8,17)
Power2(131,3)

#d)
Power3 = function(x,a){return(x^a)}

#e)
plot(1:10,Power3(1:10,2), xlab="x", ylab="y", main="y=x^2")

#f)
PlotPower = function(x,a){plot(x,x^a,xlab="x",ylab="y")}

#13)
b=Boston
attach(b)
crim01 = as.numeric(crim>median(crim))
plot(as.factor(crim01)~as.factor(chas))
par(mfrow=c(3,4))
for(i in 2:14){
  if (i!=4){
    #excludes chas
    plot(as.factor(crim01)~b[,i],xlab=names(b)[i],main=names(b)[i])
  }
}

set.seed(7)
train=runif(dim(b)[1])>0.3

#Logistic regression
glm.fit=glm(crim01~zn+indus+nox+rad+dis+tax+ptratio,family=binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep(0,dim(b)[1])
glm.pred[glm.probs>.5]=1
mean(glm.pred[!train]==crim01[!train])
#0.9109

#LDA
lda.fit=lda(crim01~zn+indus+nox+rad+dis+tax+ptratio,subset=train)
lda.pred=predict(lda.fit, b[!train,])
mean(lda.pred$class==b[!train,]$crim01)
#0.8288

#KNN
trainIn=cbind(zn[train],indus[train],nox[train],dis[train],tax[train],ptratio[train])
test=cbind(zn[!train],indus[!train],nox[!train],dis[!train],tax[!train],ptratio[!train])

trainOut=crim01[train]
knn.pred=knn(trainIn,test,trainOut,k=1)
mean(knn.pred==crim01[!train])

knn_accuracy_by_k=sapply(seq(1,21,2),function(i) mean(knn(trainIn,test,trainOut,k=i)==crim01[!train]))
par(mfrow=c(1,1))
plot(knn_accuracy_by_k~seq(1,21,2),xlab="K",ylab="accuracy",main="KNN Crime prediction accuracy by k")

mean(knn(trainIn,test,trainOut,k=3)==crim01[!train])