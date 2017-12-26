leaf.tr=read.csv('/Users/feizysmac/Dropbox/8330/Project2/train.csv')
leaf.te=read.csv('/Users/feizysmac/Dropbox/8330/Project2/test.csv')
#logloss
logLoss = function(pred, origin){
  -1*mean(log(pred[model.matrix(~ origin + 0) - pred > 0]))
}

library(randomForest)
attach(leaf.tr)
test.id=leaf.te$id
tr.value=leaf.tr$species
set.seed(1)
fold=sample(1:10,nrow(leaf.tr),replace = T)

#randomForest
#misclassification rate based on traning set
mis.rate=rep(NA,10)
nodsize=c(1,2,3,4,5)#j
mtry=c(10,15,20,25,30)#k
score=array(NA,dim=c(5,5))
logloss.rf=array(NA,dim=c(5,5))
for(j in 1:5){
  for(k in 1:5){
    for(i in 1:10){
      fit.tr=randomForest(species~.-id,data = leaf.tr[fold!=i,],ntree=500,mtry=mtry[k],nodesize=nodsize[j])
      pred.tr=predict(fit.tr,newdata = leaf.tr[fold==i,])
      pred.prob=predict(fit.tr,newdata = leaf.tr[fold==i,],type='prob')
      mis.rate[i]=1-sum(diag(table(pred.tr,leaf.tr[fold==i,]$species)))/nrow(leaf.tr[fold==i,])
      score[j,k]=mean(mis.rate)
      logloss.rf[j,k]=logLoss(pred = pred.prob,origin = pred.tr)
    }
  }
}
score2=rep(NA,7)
ntree=c(100,200,300,400,500,600,700)
for(k in 1:7){
  for(i in 1:10){
    fit.tr=randomForest(species~.-id,data = leaf.tr[fold!=i,],ntree=ntree[k],mtry=20,nodesize=1)
    pred.tr=predict(fit.tr,newdata = leaf.tr[fold==i,])
    mis.rate[i]=1-sum(diag(table(pred.tr,leaf.tr[fold==i,]$species)))/nrow(leaf.tr[fold==i,])
    score2[k]=mean(mis.rate)
  }
}
#based on test set  score=0.66233  logloss=0.1962669  misrate=0
rf.fit.te=randomForest(species~.-id,data = leaf.tr,ntree=600,mtry=20,nodesize=1)
rfpred.te=predict(rf.fit.te,newdata = leaf.te,type = 'prob')
result1=cbind(leaf.te$id,rfpred.te)
write.csv(result1,file = 'rf.csv',row.names = F)

rfpred.tr=predict(rf.fit.te,newdata=leaf.tr,type='prob')
loss.rf=logLoss(pred = rfpred.tr,origin = leaf.tr$species)
pred.rf.tr=predict(rf.fit.te,newdata=leaf.tr)
misrate.rf=1-sum(diag(table(pred.rf.tr,leaf.tr$species)))/nrow(leaf.tr)




#knn
#based on training set
library(kknn)
score=rep(NA,10)
for(k in 1:10){
  for(i in 1:10){
    fit.tr=kknn(species~.-id,train = leaf.tr[fold!=i,],test = leaf.tr[fold==i,], k = k)
    mis.rate[i]=1-sum(diag(table(fit.tr$fitted.values,leaf.tr[fold==i,]$species)))/nrow(leaf.tr[fold==i,])
  }
  score[k]=mean(mis.rate)
}
#based on test set k=3 score=0.07948  logloss=0.1768586  misrate=0
fit.te=kknn(species~.-id,train = leaf.tr,test = leaf.te, k = 3)
result2=cbind(leaf.te$id,fit.te$prob)
fit.tr=kknn(species~.-id,train = leaf.tr,test = leaf.tr, k = 3)
loss.knn=logLoss(pred = fit.tr$prob,origin =leaf.tr$species )
misrate.knn=1-sum(diag(table(fit.tr$fitted.values,leaf.tr$species)))/nrow(leaf.tr)
write.csv(result2,file = 'knn.csv',row.names = F)

#svm
#based on training data        logloss=4.595203   misrate= 0
cost=10^seq(-2,1,length=10)
for(k in 1:10){
  for(i in 1:10){
    fit.tr=svm(species~.-id,data = leaf.tr[fold!=i,],scale = T, cost=cost[k])
    pred.tr=predict(fit.tr,newdata = leaf.tr[fold==i,])
    mis.rate[i]=1-sum(diag(table(pred.tr,leaf.tr[fold==i,]$species)))/nrow(leaf.tr[fold==i,])
  }
  score[k]=mean(mis.rate)
}
#based on test data
fit.svm=svm(species~.-id,data = leaf.tr,scale = T, kernal= 'radial',cost=0.1,gamma=0.5,probability=T)
pred.svm=predict(fit.svm,newdata=leaf.te,probability = T)
pred.svm.tr=predict(fit.svm,newdata=leaf.tr,probability = T)
pred.svm.tr2=predict(fit.svm,newdata=leaf.tr,probability = F)
result3=cbind(leaf.te$id,attr(pred.svm, "probabilities"))
loss.svm=logLoss(pred = attr(pred.svm.tr, "probabilities"),origin = leaf.tr$species)
write.csv(result3,file = 'svm_radial.csv',row.names = F)
misrate.svm=1-sum(diag(table(pred.svm.tr2,leaf.tr$species)))/nrow(leaf.tr)
