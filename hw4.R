#Assignment 4
#Mose Wintner

#9
library(ISLR)
library(caret) #Hope it's OK that I use this
library(leaps)
library(car)
library(elasticnet)



#a
set.seed(432)
trainIndex = createDataPartition(1:dim(College)[1],p=.8,list=F,times=1)
training = College[trainIndex,]
testing = College[-trainIndex,]

#b
lm.fit = lm(Apps~.,data=training)

sum((predict(lm.fit,newdata=testing)-testing$Apps)^2)
#[1] 331688921

#RMSE
sqrt(sum((predict(lm.fit,newdata=testing)-testing$Apps)^2)/(dim(testing)[1]))
#[1] 1472.38



#c
#20-fold cross validation
set.seed(431)
rrControl=trainControl(method="LGOCV",number=20)
rr.fit=train(Apps~.,data=training,
             method="ridge",
             trControl=rrControl,
             tuneGrid=expand.grid(.lambda=10^(seq(-4,1,.1))),
             selectionFunction="best")
rr.fit
#RMSE was used to select the optimal model using  the smallest value.
#The final value used for the model was lambda = 0.005011872. 
#  lambda        RMSE      Rsquared 
# 5.011872e-03   998.1292  0.9218481


#RMSE
sqrt(sum((predict(rr.fit,newdata=testing)-testing$Apps)^2)/(dim(testing)[1]))
#[1] 1532.254



#d
set.seed(431)
lassoControl=trainControl(method="cv",number=20)
lasso.fit=train(Apps~.,data=n,
                method="lasso",
                trControl=lassoControl,
                tuneGrid = expand.grid(fraction=seq(0.1,1,.1)))
lasso.fit
# Below, fraction refers to the fraction of the original 
#RMSE was used to select the optimal model using  the smallest value.
#The final value used for the model was fraction = 0.8. 
#  fraction  RMSE      Rsquared 
# 0.8        989.5594  0.9230732

#RMSE
sqrt(sum((predict(lasso.fit,newdata=testing)-testing$Apps)^2)/(dim(testing)[1]))
#[1] 1501.093



predict(lasso.fit$finalModel,s=.8, type ="coefficients", mode ="fraction")$coef
# PrivateYes
# -5.472550e+02
# Accept
# 1.176054e+00
# Enroll
# 0.000000e+00
# Top10perc
# 3.840194e+01
# Top25perc
# -7.193605e+00
# F.Undergrad
# 7.350860e-02
# P.Undergrad
# 1.697092e-02
# Outstate
# -2.828303e-02
# Room.Board
# 1.574360e-01
# Books
# 0.000000e+00
# Personal
# 5.917054e-03
# PhD
# -2.769126e+00
# Terminal
# -5.994969e+00
# S.F.Ratio
# 0.000000e+00
# perc.alumni
# -3.577691e+00
# Expend
# 7.124398e-02

# Thus there are 13 nonzero coefficients.

#e
set.seed(431)
pcrControl=trainControl(method="LGOCV",number=30)
pcr.fit=train(Apps~.,data=training,
                method="pcr",
                trControl=pcrControl,
                tuneGrid = expand.grid(ncomp=1:13))
pcr.fit
#RMSE was used to select the optimal model using  the smallest value.
#The final value used for the model was ncomp = 11. 
# ncomp  RMSE      Rsquared  
# 11     1014.691  0.91371284


#RMSE
sqrt(sum((predict(pcr.fit,newdata=testing)-testing$Apps)^2)/(dim(testing)[1]))
#[1] 1455.188



#f
set.seed(431)
plsControl=trainControl(method="LGOCV",number=30)
pls.fit=train(Apps~.,data=training,
              method="pls",
              trControl=plsControl,
              tuneGrid = expand.grid(ncomp=1:13))

pls.fit
#RMSE was used to select the optimal model using  the smallest value.
#The final value used for the model was ncomp = 13.
#ncomp  RMSE      Rsquared 
#13     1004.561  0.9206768

#RMSE
sqrt(sum((predict(pls.fit,newdata=testing)-testing$Apps)^2)/(dim(testing)[1]))
#[1] 1465.355


#g
# There is not much difference in the error. pcr barely does best on the testing set,
# but the lasso has the lowest cross-validation error. None of them really do such a great job.


#11
#a
set.seed(432)
trainIndex = createDataPartition(1:dim(Boston)[1],p=.8,list=F,times=1)
training = Boston[trainIndex,]
testing = Boston[-trainIndex,]

control=trainControl(method="LGOCV",number=30)
rr.fit=train(crim~.,data=training,
             method="ridge",
             trControl=control,
             tuneGrid=expand.grid(.lambda=10^(seq(-4,1,.1))))
lasso.fit=train(crim~.,data=training,
                method="lasso",
                trControl=control,
                tuneGrid = expand.grid(fraction=seq(0.1,1,.1)))
pcr.fit=train(crim~.,data=training,
              method="pcr",
              trControl=control,
              tuneGrid = expand.grid(ncomp=1:13))
rf.fit=train(crim~.,data=training,
             method="rf",
             trControl=control,
             tuneGrid =expand.grid(mtry=2:10))
rf.fit
#The final value used for the model was mtry = 2. 


bestsubset=regsubsets(x=Boston[,-which(names(Boston)=="crim")],
                      y=Boston$crim,
                      method="exhaustive",
                      all.best=T)
summary(bestsubset)
# 1 subsets of each size up to 8
# Selection Algorithm: exhaustive
# zn  indus chas nox rm  age dis rad tax ptratio black lstat medv
# 1  ( 1 ) " " " "   " "  " " " " " " " " "*" " " " "     " "   " "   " " 
# 2  ( 1 ) " " " "   " "  " " " " " " " " "*" " " " "     " "   "*"   " " 
# 3  ( 1 ) " " " "   " "  " " " " " " " " "*" " " " "     "*"   "*"   " " 
# 4  ( 1 ) "*" " "   " "  " " " " " " "*" "*" " " " "     " "   " "   "*" 
# 5  ( 1 ) "*" " "   " "  " " " " " " "*" "*" " " " "     "*"   " "   "*" 
# 6  ( 1 ) "*" " "   " "  "*" " " " " "*" "*" " " " "     "*"   " "   "*" 
# 7  ( 1 ) "*" " "   " "  "*" " " " " "*" "*" " " "*"     "*"   " "   "*" 
# 8  ( 1 ) "*" " "   " "  "*" " " " " "*" "*" " " "*"     "*"   "*"   "*" 

plotted=subsets(bestsubset, statistic="bic")
#looks like the best by Bayes Information is rad, black, lstat

sqrt(sum((predict(rr.fit,newdata=testing)-testing$crim)^2)/(dim(testing)[1]))
#[1] 8.400772
sqrt(sum((predict(lasso.fit,newdata=testing)-testing$crim)^2)/(dim(testing)[1]))
#[1] 8.430356
sqrt(sum((predict(pcr.fit,newdata=testing)-testing$crim)^2)/(dim(testing)[1]))
#[1] 8.291054
sqrt(sum((predict(rf.fit,newdata=testing)-testing$crim)^2)/dim(testing)[1])
#[1] 7.734899

#b
#Random forest does best. This makes sense because a partition of the data space is likely to predict crime rate well,
#since groups of statistics probably are capable of capturing characteristics of a community associated with crime

#c
#Yes it does...the tree appears to be fairly deep. It seems like Random Forest should overfit but it's based on the bootstrap
#so I guess it doesn't.