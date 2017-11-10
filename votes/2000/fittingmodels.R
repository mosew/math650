#############################################
############# FITTING MODELS ################


library(glmnet)
library(MASS)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(randomForest)


# Load and prepare data
source('C:/Users/mose/Dropbox/HW/math650/votes/2000/proj.R')



############## REGRESSIONS ##################


################ LASSO ######################

lasso.control = trainControl(method="cv",number=20,selectionFunction="oneSE")
set.seed(666)
lasso.fit = train(rpct~., data=n,
                  method="glmnet",
                  trControl=lasso.control,
                  preProc=c("center","scale"),
                  tuneGrid=expand.grid(alpha=1,lambda=10^seq(-3,0,.01)))

lasso.fit

lasso.imp=varImp(lasso.fit)$importance
lasso.imp



########## PLOT ############

lasso.fm=lasso.fit$finalModel
x.full=lasso.fm$df
y.full=lasso.fm$dev.ratio
keep.full=x.full[2:length(x.full)]!=x.full[1:length(x.full)-1]
ggplot()+geom_line(aes(x=x.full[keep.full],y=y.full[keep.full]))+
  labs(title="lasso on full dataset",x="number of variables",y="pct deviance explained")
  


###### LASSO COEFFICIENTS #####

b=as.matrix(coef(lasso.fm,lasso.fit$bestTune$lambda,alpha=1))

sum(b==0)

row.names(b)[b==0]

b=b[b[,1]!=0,,drop=FALSE]
b[order(-b[,1]),,drop=FALSE]





########## BAGGED TREES ###########

treebag.control=trainControl(method="boot",number=100)
set.seed(323)
treebag.fit=train(rpct~.,data=n,
                  method="treebag",
                  trControl=treebag.control)


treebag.fit

treebag.imp=varImp(treebag.fit)$importance



######### RANDOM FOREST ###########


rf.control=trainControl(method="cv",number=10)
set.seed(323)
rf.fit=train(rpct ~ ., data=n,
             method="rf",
             importance=T,
             verbose=F,
             ntree=400,
             trControl=rf.control,
             tuneGrid=data.frame(mtry=30),
             allowParallel=T,
             do.trace=100)


rf.fm=rf.fit$finalModel
rf.fm
# % Var explained: 72.34


rf.imp=varImp(rf.fit)$importance




######### STOCHASTIC GRADIENT BOOSTING / BOOSTED TREES ############

gbm.control = trainControl(method="cv",number=10)
set.seed(46)
gbm.fit = train(rpct~., data=n,
                method="gbm",
                trControl=gbm.control,
                verbose=F,
                tuneGrid=expand.grid(n.trees=450,
                                     interaction.depth=11,
                                     shrinkage=.1,
                                     n.minobsinnode=20))

gbm.imp=varImp(gbm.fit)$importance

gbm.fm=gbm.fit$finalModel


################# LINEAR SVM #################

svm.control=trainControl(method="cv",number=10)
set.seed(489392)
svm.fit = train(rpct~.,data=n,
                method="svmLinear",
                trControl=svm.control,
                tuneGrid=data.frame(C=2^(-2:0)))

svm.imp=varImp(svm.fit)$importance

svm.fm=svm.fit$finalModel




######## xgBOOST LINEAR ##################
xgbl.control=trainControl(method="cv",number=10)
set.seed(323)
xgbl.fit = train(rpct~.,data=n,
                method="xgbLinear",
                trControl=xgbl.control,
                tuneGrid=expand.grid(nrounds=seq(150,250,50),lambda=seq(0.1,0.3,0.1),alpha=0.1,eta=0.3))

xgbl.imp=varImp(xgbl.fit)$importance

xgbl.fm=xgbl.fit$finalModel


############ xgBOOST TREE ################
xgbt.control=trainControl(method="cv",number=10)
set.seed(323)
xgbt.fit = train(rpct~.,data=n,
                method="xgbTree",
                trControl=xgbt.control,
                tuneGrid=data.frame(nrounds=c(150,200,250),
                                    max_depth=3:5,
                                    eta=0.3,gamma=0,
                                    colsample_bytree=0.6,min_child_weight=1))

xgbt.imp=varImp(xgbt.fit)$importance

xgbt.fm=xgbt.fit$finalModel