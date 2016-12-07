leaf.tr=read.csv('C:\\Users\\feizy\\Documents\\R\\train.csv')
leaf.te=read.csv('C:\\Users\\feizy\\Documents\\R\\test.csv')
library(randomForest)
attach(leaf.tr)
test.id=leaf.te$id
tr.value=leaf.tr$species
fold=sample(1:10,nrow(leaf.tr),replace = T)
mis.rate=rep(NA,10)
#misclassification rate based on traning set
for(i in 1:10){
  fit.tr=randomForest(species~.-id,data = leaf.tr[fold!=i,],ntree=500)
  pred.tr=predict(fit.tr,newdata = leaf.tr[fold==i,])
  mis.rate[i]=1-sum(diag(table(pred.tr,leaf.tr[fold==i,]$species)))/nrow(leaf.tr[fold==i,])
  
}

#based on test set
fit.te=randomForest(species~.-id,data = leaf.tr,ntree=500)
pred.te=predict(fit.te,newdata = leaf.te)
result=cbind(leaf.te$id,pred.te)
write.csv(result,file = 'leaf1.csv',row.names = F)