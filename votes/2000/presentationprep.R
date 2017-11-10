########## PRESENTATION ###########

require(ggplot2)
library(gridExtra)

model.names=c("Lasso","Random Forest","SVM","GBM","Bagged CART","XGBLinear", "XGBTree")


############### CALCULATING VARIABLE IMPORTANCE #####################

########## PCTS #############
pcts.importances = data.frame(
                    t(merge(t(lasso.imp),
                           merge(t(rf.imp),
                                 merge(t(svm.imp),
                                       merge(t(gbm.imp),
                                             merge(t(treebag.imp),
                                                   merge(t(xgbl.imp),t(xgbt.imp),all=T),
                                             all=T),
                                       all=T),
                                 all=T),
                           all=T),
                     all=T)))

names(pcts.importances)=model.names
pcts.importances=format(pcts.importances[order(-pcts.importances[,4]),],scientific=F)


pcts.results = rbind(lasso.fit$results[203,c(4,6,3)],rf.fit$results[,c(3,5,2)],
                          svm.fit$results[2,c(3,5,2)],gbm.fit$results[,c(6,8,5)],
                          treebag.fit$results[,c(3,5,2)],
                          xgbl.fit$results[7,c(6,8,5)],xgbt.fit$results[2,c(8,10,7)])

pcts.results=data.frame(t(pcts.results))
names(pcts.results)=model.names

pcts.table=rbind(pcts.results,pcts.importances)
r2=as.vector(t(pcts.results[1,]))
r2=(r2-min(r2))
r2=r2/max(r2)

pcts.score=data.matrix(pcts.importances)%*%r2
pcts.score=pcts.score[order(-pcts.score),]


p=pcts.table[-c(2,3),-1]

################ LN OF VOTE COUNTS ##################

counts.importances = t(data.frame(
  #t(merge(t(lasso2.imp),
          merge(t(rf2.imp),
   #             merge(t(svm2.imp),
                      merge(t(gbm2.imp),
                            merge(t(treebag2.imp),
                                  #merge(t(xgbl2.imp),
                                        t(xgbt2.imp),all=T),
                                  #all=T),
                        #    all=T),
                      #all=T),
                all=T),
          all=T)))

names(counts.importances)=model.names
counts.importances=format(counts.importances[order(-counts.importances[,1]),],scientific=F)



counts.results = rbind(
  #lasso2.fit$results[14,c(4,6)],
                       rf2.fit$results[,c(3,5)],
                     #svm2.fit$results[2,c(3,5)],
                     gbm2.fit$results[7,c(6,8)],
                     treebag2.fit$results[,c(3,5)],
                     #xgbl2.fit$results[6,c(6,8)],
                     xgbt2.fit$results[2,c(8,10)])
counts.results=data.frame(t(counts.results))
names(counts.results)=model.names

counts.table=rbind(counts.results,counts.importances)
r2.2=as.vector(t(counts.results[1,]))
r2.2=r2.2-min(r2.2)
r2.2=r2.2/max(r2.2)
counts.score=data.matrix(counts.importances)%*%data.matrix(r2.2)
counts.score=counts.score[order(-counts.score),]


####################### COUNTS [ACTUAL] ####################

actual.importances = t(data.frame(merge(t(varImp(poisson.fit)$importance),
                                      merge(t(rf3.imp),
                                        merge(t(gbm3.imp),
                                          merge(t(treebag3.imp),t(xgbt3.imp),all=T),
                                        all=T),
                                      all=T),
                                    all=T)
                                ))

model.names2=c("Poisson",model.names[c(2,4,5,7)])
#actual.importances=format(actual.importances[order(-actual.importances[,1]),],scientific=F)



actual.results = rbind(poisson.fit$results[,c(3,5)],rf3.fit$results[3,c(3,5)],gbm3.fit$results[1,c(6,8)],
                       treebag3.fit$results[,c(3,5)],xgbt3.fit$results[3,c(8,10)])
actual.results=data.frame(t(actual.results))
names(actual.results)=model.names2

r2.2=as.vector(t(actual.results[1,]))
r2.2=r2.2-min(r2.2)
r2.2=data.matrix(r2.2/max(r2.2))
actual.score=data.matrix(actual.importances)%*%data.matrix(r2.2)
actual.score=data.frame(actual.score[order(-actual.score),])



########################### CLASSIFICATION ###########################

class.model.names=c("SVM","LDA","QDA","KNN","Adaboost")

class.results = rbind(svm.cl.fit$results[,c(2,3)],
                       lda.fit$results[,c(2,3)],
                       qda.fit$results[,c(2,3)],
                       knn.fit$results[,c(2,3)],
                       ada.fit$results[,c(4,5)])
class.results=data.frame(t(class.results))
names(class.results)=class.model.names
class.results=t(class.results)


####################### FLORIDA ########################

# training = t$state!="FL"
# adafl.control=trainControl(method="cv",number=20)
# set.seed(661)
# adafl.fit=train(rpct~.,data=n.2,
#                 subset=training,
#               method="ada",
#               trControl=adafl.control,
#               tuneGrid=data.frame(iter=250,maxdepth=6,nu=0.1))
# 
# fl.predict=predict(adafl.fit,newdata=n.2[t$state=="FL",])






########################## PLOTTING INTERESTING VARIABLES #####################


plot.br = function(name){
  ind=which(names(t)==paste(name))
  p=ggplot()+geom_freqpoly(aes(x=t[,ind][t$rpct>50]),bins=100,color="red")+
    geom_freqpoly(aes(x=t[,ind][t$rpct<50]),color="blue",bins = 100)+
    labs(title=c(paste(name)," vs count"),x="",y="number of counties")
  p
}



grid.arrange(plot.br("latitude"),
             plot.br("lnpoppersqm"),
             plot.br("married"),
             plot.br("latinamerican"),nrow=2,ncol=2)
