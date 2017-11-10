#############################################
############# FITTING MODELS ################


library(flexclust)
library(MASS)
library(ggplot2)
library(caret)
library(dplyr)


# Load and prepare data
source('C:/Users/mose/Dropbox/HW/math650/votes/2000/proj.R')


############## CLASSIFICATION ##################

################# SVM ###################

svm.cl.control=trainControl(method="cv",number=10)
set.seed(323)
svm.cl.fit=train(rpct~.,data=n.2,
                 method="svmLinear",
                 trControl=svm.cl.control,
                 tuneGrid=data.frame(C=1))

svm.cl.imp=varImp(svm.cl.fit)$importance


################# LDA ###################

lda.control=trainControl(method="boot",number=10)
set.seed(323)
lda.fit=train(rpct~.,data=n.2,
              method="lda",
              trControl=lda.control)


################ QDA ####################

qda.control=trainControl(method="boot",number=10)
set.seed(323)
qda.fit=train(rpct~.,data=n.2,
              method="qda",
              trControl=qda.control)


################# KNN ####################
knn.control=trainControl(method="cv",number=10)
set.seed(323)
knn.fit=train(rpct~.,data=n.2,
              method="knn",
              trControl=knn.control,
              tuneGrid=data.frame(k=30))

knn.imp=varImp(knn.fit)$importance

############## ADABOOST ###################

ada.control=trainControl(method="cv",number=20)
set.seed(323)
ada.fit=train(rpct~.,data=n.2,
              method="ada",
              trControl=ada.control,
              tuneGrid=data.frame(iter=250,maxdepth=6,nu=0.1))




# ########## CLUSTER ANALYSIS #################
# 
# set.seed(323)
# k.clusters=flexclust:::kcca(n,k=5,family=kccaFamily("kmedians"))
