#############################################
############# FITTING MODELS ################


# Fitting the logarithm of the number of votes for Bush.



library(glmnet)
library(glm2)
library(MASS)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(randomForest)


# Load and prepare data
source('C:/Users/mose/Dropbox/HW/math650/votes/2000/proj.R')





############## REGRESSIONS ##################

glm = function(...,method=glm2::glm.fit2){
  stats::glm(...,method=method)
}

########### MAKE NEW LOGRVOTES VARIABLES ##########

m$logrvotes = log(m$rvotes+1)
m$rvotes=NULL
#m$clogrvotes = m$logrvotes-cor(m$logrvotes,m$lnpop)*m$lnpop

which(names(m)=="logrvotes")

################ LASSO ######################

lasso2.control = trainControl(method="cv",number=20,selectionFunction="oneSE")
set.seed(666)
lasso2.fit = train(logrvotes~., data=m,
                  method="glmnet",
                  trControl=lasso2.control,
                  preProc=c("center","scale"),
                  tuneGrid=expand.grid(alpha=1,lambda=10^seq(-4,0,.1)))


lasso2.imp=varImp(lasso2.fit)$importance
lasso2.imp


###### LASSO COEFFICIENTS #####

b2=as.matrix(coef(lasso2.fm,lasso2.fit$bestTune$lambda,alpha=1))

sum(b2==0)

row.names(b2)[b2==0]

b2=b2[b2[,1]!=0,,drop=FALSE]
b2[order(-b2[,1]),,drop=FALSE]





########## BAGGED TREES ###########

treebag2.control=trainControl(method="boot",number=100)
treebag2.fit=train(logrvotes~.,data=m,
                  method="treebag",
                  trControl=treebag2.control)


treebag2.fit

treebag2.imp=varImp(treebag2.fit)$importance
treebag2.imp



treebag2.fm=treebag2.fit$finalModel
treebag2.fm


######### RANDOM FOREST ###########


rf2.control=trainControl(method="cv",number=10)
set.seed(323)
rf2.fit=train(logrvotes ~ ., data=m,
             method="rf",
             importance=T,
             verbose=F,
             ntree=400,
             trControl=rf2.control,
             tuneGrid=data.frame(mtry=30),
             allowParallel=T,
             do.trace=100)




rf2.fm=rf2.fit$finalModel
rf2.fm
# Mean of squared residuals: 39.37243
# % Var explained: 72.52


rf2.imp=varImp(rf2.fit)$importance
rf2.imp

# 
# rf2imp20=rf.imp[order(-rf.imp[,2]), ,drop=FALSE][1:20, ,drop=FALSE]
# rf2imp20
# rf2imp20names=row.names(rfimp20)
# 
# rf2implow20=rf.imp[order(rf.imp[,2]), ,drop=FALSE][1:20, ,drop=FALSE]




######### STOCHASTIC GRADIENT BOOSTING / BOOSTED TREES ############

gbm2.control = trainControl(method="cv",number=10)
set.seed(46)
gbm2.fit = train(logrvotes~., data=m,
                method="gbm",
                trControl=gbm2.control,
                verbose=F,
                tuneGrid=expand.grid(n.trees=c(400,450,500),
                                     interaction.depth=c(9,11,13,15),
                                     shrinkage=.1,
                                     n.minobsinnode=20))

gbm2.imp=varImp(gbm2.fit)$importance
gbm2.imp


gbm2.fm=gbm2.fit$finalModel


################# LINEAR SVM #################

svm2.control=trainControl(method="cv",number=10)
set.seed(489392)
svm2.fit = train(logrvotes~.,data=m,
                method="svmLinear",
                trControl=svm2.control,
                tuneGrid=data.frame(C=2^(-2:0)))

svm2.imp=varImp(svm2.fit)$importance
svm2.imp




######## xgBOOST LINEAR ##################
xgbl2.control=trainControl(method="cv",number=10)
set.seed(323)
xgbl2.fit = train(logrvotes~.,data=m,
                 method="xgbLinear",
                 trControl=xgbl2.control,
                 tuneGrid=expand.grid(nrounds=seq(150,250,50),lambda=seq(0.1,0.3,0.1),alpha=0.1,eta=0.3))
xgbl2.fit

xgbl2.imp=varImp(xgbl2.fit)$importance

xgbl2.fm=xgbl2.fit$finalModel


############ xgBOOST TREE ################
xgbt2.control=trainControl(method="cv",number=10)
set.seed(323)
xgbt2.fit = train(logrvotes~.,data=m,
                 method="xgbTree",
                 trControl=xgbt2.control,
                 tuneGrid=data.frame(nrounds=c(150,200,250),
                                     max_depth=3:5,
                                     eta=0.3,gamma=0,
                                     colsample_bytree=0.6,min_child_weight=1))
xgbt2.fm=xgbt2.fit$finalModel
xgbt2.imp=varImp(xgbt2.fit)$importance





######### AGGREGATION ###########


plot_bvg = function(name){
  ind=which(names(t)==paste(name))
  ggplot()+geom_freqpoly(aes(x=t[,ind][t$log(rvotes + 1)>50]),bins=100,color="red")+
    geom_freqpoly(aes(x=t[,ind][t$log(rvotes + 1)<50]),color="blue",bins = 100)+
    labs(title=c(paste(name)," vs count"),xlab=c(paste(name)),ylab="count")
}
