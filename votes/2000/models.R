#############################################
############# FITTING MODELS ################


require(glmnet)
require(MASS)
require(ggplot2)
require(plyr)
require(dplyr)
require(caret)
require(randomForest)


# Load and prepare data
source('C:/Users/mose/Dropbox/math650/votes/2000/preprocess.R')


############## REGRESSIONS ##################


################ LASSO ######################

lasso.control = trainControl(method="cv",number=20,selectionFunction="oneSE")
set.seed(666)
lasso.fit = train(rpct~., data=X,
                  method="glmnet",
                  trControl=lasso.control,
                  preProcess=c("center","scale"),
                  tuneGrid=expand.grid(alpha=1,lambda=10^seq(-3,-1,.01)))

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





######### RANDOM FOREST ###########

rf.control=trainControl(method="cv",number=5)
set.seed(323)
rf.fit=train(rpct ~ ., data=X,
             method="rf",
             importance=T,
             verbose=F,
             ntree=500,
             trControl=rf.control,
             tuneGrid=data.frame(mtry=25),
             allowParallel=T,
             do.trace=100)


rf.fm=rf.fit$finalModel
rf.fm
# % Var explained: 72.34


rf.imp=varImp(rf.fit)$importance




######### STOCHASTIC GRADIENT BOOSTING / BOOSTED TREES ############

gbm.control = trainControl(method="cv",number=6)
set.seed(46)
gbm.fit = train(rpct~., data=X,
                method="gbm",
                trControl=gbm.control,
                verbose=F,
                tuneGrid=expand.grid(n.trees=450,
                                     interaction.depth=13,
                                     shrinkage=.1,
                                     n.minobsinnode=20))

gbm.fm=gbm.fit$finalModel


############# CLASSIFICATIONS ################

############# QDA ############

qda.control=trainControl(method="cv",number=4)
set.seed(489392)
qda.fit = train(rpct~.,data=X.2,
                     method="qda",
                     trControl=qda.control,
                     metric = "Kappa")

qda.imp=varImp(qda.fit)$importance

qda.fm=qda.fit$finalModel


################# AdaBoost #################

adaboost.control=trainControl(method="cv",number=4)
set.seed(489392)
adaboost.fit = train(rpct~.,data=X.2,
                method="adaboost",
                trControl=adaboost.control,
                tuneGrid=expand.grid(nIter=3,method="ada"),
                metric = "Kappa")

adaboost.imp=varImp(adaboost.fit)$importance

adaboost.fm=adaboost.fit$finalModel



############# KNN ######################

knn.control=trainControl(method="cv",number=8)
set.seed(323)
knn.fit = train(rpct~.,
                data = X.2,
                method = "knn",
                trControl=knn.control,
                tuneGrid=expand.grid(k=c(6,8,10)),
                metric="Kappa")
