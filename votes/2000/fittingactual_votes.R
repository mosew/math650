# FITTING VOTES


# Ill-advised from the start, but one could also perform a regression on the actual NUMBER of votes per county given census data.


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





################ POISSON ###################

poisson.control = trainControl(method="cv",number=10)
set.seed(323)
poisson.fit = train(rvotes~.,data=m,method="glm",family="poisson")

########## BAGGED TREES ###########

treebag3.control=trainControl(method="cv",number=10)
treebag3.fit=train(rvotes~.,data=m,
                   method="treebag",
                   trControl=treebag3.control)


treebag3.imp=varImp(treebag3.fit)$importance

######### RANDOM FOREST ###########


rf3.control=trainControl(method="cv",number=10)
set.seed(323)
rf3.fit=train(rvotes ~ ., data=m,
              method="rf",
              importance=T,
              verbose=F,
              trControl=rf3.control,
              tuneGrid=data.frame(mtry=seq(10,40,10)),
              do.trace=100)




rf3.fm=rf3.fit$finalModel
rf3.fm
# Mean of squared residuals: 39.37243
# % Var explained: 72.52


rf3.imp=varImp(rf3.fit)$importance
rf3.imp

# 
# rf3imp20=rf.imp[order(-rf.imp[,2]), ,drop=FALSE][1:20, ,drop=FALSE]
# rf3imp20
# rf3imp20names=row.names(rfimp20)
# 
# rf3implow20=rf.imp[order(rf.imp[,2]), ,drop=FALSE][1:20, ,drop=FALSE]




######### STOCHASTIC GRADIENT BOOSTING / BOOSTED TREES ############

gbm3.control = trainControl(method="cv",number=10)
set.seed(46)
gbm3.fit = train(rvotes~., data=m,
                 method="gbm",
                 trControl=gbm3.control,
                 verbose=F,
                 tuneGrid=expand.grid(n.trees=c(200,400,50),
                                      interaction.depth=c(3,5,7,9),
                                      shrinkage=.1,
                                      n.minobsinnode=20))

gbm3.imp=varImp(gbm3.fit)$importance
gbm3.imp


gbm3.fm=gbm3.fit$finalModel

############ xgBOOST TREE ################
xgbt3.control=trainControl(method="cv",number=10)
set.seed(323)
xgbt3.fit = train(rvotes~.,data=m,
                  method="xgbTree",
                  trControl=xgbt3.control,
                  tuneGrid=data.frame(nrounds=c(150,200,250),
                                      max_depth=3:5,
                                      eta=0.3,gamma=0,
                                      colsample_bytree=0.6,min_child_weight=1))
xgbt3.fm=xgbt3.fit$finalModel
xgbt3.imp=varImp(xgbt3.fit)$importance
