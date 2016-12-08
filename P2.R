leaf.tr=read.csv('C:\\Users\\feizy\\Documents\\R\\train.csv')
leaf.te=read.csv('C:\\Users\\feizy\\Documents\\R\\test.csv')
library(randomForest)
attach(leaf.tr)
test.id=leaf.te$id
tr.value=leaf.tr$species
fold=sample(1:10,nrow(leaf.tr),replace = T)

#randomForest
#misclassification rate based on traning set
mis.rate=rep(NA,10)
nodsize=c(1,2,3,4,5)#j
mtry=c(10,15,20,25,30)#k
score=array(NA,dim=c(5,5))
for(j in 1:5){
  for(k in 1:5){
    for(i in 1:10){
      fit.tr=randomForest(species~.-id,data = leaf.tr[fold!=i,],ntree=500,mtry=mtry[k],nodesize=nodsize[j])
      pred.tr=predict(fit.tr,newdata = leaf.tr[fold==i,])
      mis.rate[i]=1-sum(diag(table(pred.tr,leaf.tr[fold==i,]$species)))/nrow(leaf.tr[fold==i,])
      score[j,k]=mean(mis.rate)
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
#based on test set
rf.fit.te=randomForest(species~.-id,data = leaf.tr,ntree=1500,mtry=20,nodesize=1)
rfpred.te=predict(rf.fit.te,newdata = leaf.te,type = 'prob')
result=cbind(leaf.te$id,rfpred.te)
write.csv(result,file = 'rf.csv',row.names = F)

#SVM
