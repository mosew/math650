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

tenfoldcv = trainControl(method="cv",number=10)

############## REGRESSIONS ##################


################ LASSO ######################

lasso.control = trainControl(method="repeatedcv",
                             number=10,repeats=3,selectionFunction="oneSE")
set.seed(32323)
lasso.fit = train(rpct~., data=X,
                  method="glmnet",
                  trControl=lasso.control,
                  preProcess=c("center","scale"),
                  tuneGrid=expand.grid(alpha=seq(0,1,0.1),lambda=10^seq(-3,0,.1)))

lasso.imp=data.frame(varImp(lasso.fit,scale=T)$importance)
lasso.fm=lasso.fit$finalModel

########## PLOT ############

x.full=lasso.fm$df
y.full=lasso.fm$dev.ratio
keep.full=x.full[2:length(x.full)]!=x.full[1:length(x.full)-1]
ggplot()+geom_line(aes(x=x.full[keep.full],y=y.full[keep.full]))+
  labs(title="lasso on full dataset",x="number of variables",y="pct deviance explained")
  
predict(lasso.fit,newdata=X.fl)

###### LASSO COEFFICIENTS #####

b=as.matrix(coef(lasso.fm,lasso.fit$bestTune$lambda,alpha=lasso.fit$bestTune$alpha))
#sum(b==0)
#row.names(b)[b==0]
b=b[b[,1]!=0,,drop=FALSE]
b[order(-b[,1]),,drop=FALSE]


######### RANDOM FOREST ###########

set.seed(32323)
rf.fit=train(rpct ~ ., data=X,
             method="rf",
             importance=T,
             verbose=F,
             trControl=tenfoldcv,
             #tuneGrid=data.frame(mtry=25),
             allowParallel=T,
             do.trace=100)


rf.fm=rf.fit$finalModel
rf.imp=varImp(rf.fit)$importance


############ STOCHASTIC GRADIENT BOOSTING #############

set.seed(32323)
gbm.fit = train(rpct~., data=X,
                   method="gbm",
                   trControl=tenfoldcv,
                   verbose=F,
                   tuneGrid=expand.grid(n.trees=c(400,450,500),
                                        interaction.depth=6,
                                        shrinkage=0.1,
                                        n.minobsinnode=c(10,20)))

gbm.fm=gbm.fit$finalModel
gbm.imp=varImp(gbm.fit)$importance




############### SVM (LINEAR) ###################

set.seed(32323)
svm.fit = train(rpct~., data=X,
                method="svmLinear",
                trControl=tenfoldcv,
                preProc = c("center","scale"),
                tuneGrid=expand.grid(C=c(0.1,1,2)))

svm.fm=svm.fit$finalModel
svm.imp=varImp(svm.fit)$importance


############### CART ###################

set.seed(32323)
cart.fit = train(rpct~., data=X,
                 method="rpart",
                 trControl=tenfoldcv)
cart.fm=svm2.fit$finalModel
cart.imp=varImp(svm2.fit)$importance

############### CART ###################

set.seed(32323)
knn.fit = train(rpct~., data=X,
                 method="kknn",
                 trControl=tenfoldcv)
knn.fm=knn.fit$finalModel
knn.imp=varImp(knn.fit)$importance

