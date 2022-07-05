#classification
#----------------------------------------------------------------------
#1-mer
#数据预处理
neg_1mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\neg1_mer.txt')
pos_1mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\pos1_mer.txt')
neg_1mer$V5<-0
pos_1mer$V5<-1
colnames(pos_1mer)[5]='y'
colnames(neg_1mer)[5]='y'
set.seed(1)
train_num<-sample(6606,round(6606/10*9),replace = FALSE)
pos_train_data<-pos_1mer[train_num,]
pos_test_data<-pos_1mer[-train_num,]
neg_train_data<-neg_1mer[train_num,]
neg_test_data<-neg_1mer[-train_num,]
train_data<-rbind(pos_train_data,neg_train_data)
test_data<-rbind(pos_test_data,neg_test_data)

pre_data<-data.frame(test_data$y)
colnames(pre_data)<-c("y")

#logistic回归
glm.fit<-glm(y~.,data = train_data,family = binomial)
summary(glm.fit)
glm.prob<-predict(glm.fit,test_data,type='response')
glm.pred=rep(0,1322)
glm.pred[glm.prob>0.5]=1
pre_data$logistics<-glm.pred
t.glm<-table(glm.pred,test_data$y)
(t.glm[1,1]+t.glm[2,2])/sum(t.glm)#(423+361)/(423+361+300+238)#0.59304

#线性判别回归
library(MASS)
lda.fit<-lda(y~.,data=train_data)
summary(lda.fit)
lda.pred<-predict(lda.fit,test_data[,1:4])
pre_data$lda<-lda.pred$class
t.lda<-table(lda.pred$class,test_data$y)
(t.lda[1,1]+t.lda[2,2])/sum(t.lda)#(423+361)/(423+361+300+238)#0.5930408


#随机森林模型
set.seed(2)
library('randomForest')
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优参数mtry,
n<-ncol(train_data)
rate<-1
for (i in 1:(n-1)){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
a<-which(rate==min(rate),arr.ind=TRUE)
rate[a]

#寻找最佳ntree
ntree=seq(100,1500,length=15)
rate_1<-1
for (i in 1:15){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=26,ntree=ntree[i])
  rate_1[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
b<-which(rate_1==min(rate_1),arr.ind = TRUE)
rate_1[b]

foreast.fit<-randomForest(y~.,data = train_data,mtry=32,ntree=1500)
foreast.pred<-predict(foreast.fit,test_data)
t.foreast<-table(foreast.pred,test_data$y)
(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)#(402+404)/(402+404+257+259)#0.6096823

pre_data<-cbind(pre_data,as.data.frame(foreast.pred))
colnames(pre_data)[4]<-'forest'

#svm模型
#SVM
library(e1071)
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优的gamma与cost
svm_model_tune=tune(svm,y~.,data = train_data,kernel="radial",type = "C-classification",ranges=(list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c (0.1,1,2,3,4,5,10))))
summary(svm_model_tune)
svm_fit=svm(y~.,data=train_data,cost=1,gamma=0.1,kernel="linear",type='C',probability=TRUE)

summary(svm_fit)
svm.pred=predict(svm_fit,test_data,probability = TRUE)
attr(svm.pred,"probabilities")[,1]
t.svm<-table(test_data$y,svm.pred)
(t.svm[1,1]+t.svm[2,2])/sum(t.svm)#(423+369)/(423+369+238+299)#0.5937973
pre_data<-cbind(pre_data,as.data.frame(svm.pred))
colnames(pre_data)[5]<-'svm'

pre_data$lda<-as.numeric(pre_data$lda)
pre_data$forest<-as.numeric(pre_data$forest)
pre_data$svm<-as.numeric(pre_data$svm)
pre_data[,3:5]<-pre_data[,3:5]-1
write.csv(pre_data,"C:\\Users\\87545\\Desktop\\cla_data\\1m_auc_data.csv")

auc_point_data<-pre_data
auc_point_data$logistics<-predict(glm.fit,test_data,type='response')
auc_point_data$lda<-predict(lda.fit,test_data,type='response')$posterior[,2]
auc_point_data$forest<-predict(foreast.fit,test_data,type = 'prob')[,2]
auc_point_data$svm<-attr(svm.pred,"probabilities")[,1]
write.csv(auc_point_data,"C:\\Users\\87545\\Desktop\\cla_data\\1_mer_auc_point_data.csv")

#roc计算及可视化
library(pROC)
glm.roc<-roc(auc_point_data$y,auc_point_data$logistics)#0.609
lda.roc<-roc(auc_point_data$y,auc_point_data$lda)#0.609
forest.roc<-roc(auc_point_data$y,auc_point_data$forest)#0.647
svm.roc<-roc(auc_point_data$y,auc_point_data$svm)#0.610
par(mfrow=c(1,4))
plot(glm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='Logistic')
plot(lda.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='LDA')
plot(forest.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='RandomForest')
plot(svm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='SVM')

auc_bar_data<-array(0,c(5,4))
auc_bar_data<-as.data.frame(auc_bar_data)
colnames(auc_bar_data)<-c("logistic","lda","forest","svm")
rownames(auc_bar_data)<-c("k=1",'k=2','k=3','k=4','k=5')
auc_bar_data[1,1]=ROC_logistics$auc[1]
auc_bar_data[1,2]=ROC_lda$auc[1]
auc_bar_data[1,3]=ROC_rf$auc[1]
auc_bar_data[1,4]=ROC_svm$auc[1]

acc_data<-array(0,c(5,4))
acc_data<-as.data.frame(acc_data)
colnames(acc_data)<-c("logistic",'lda','forest','svm')
rownames(acc_data)<-c('k=1','k=2','k=3','k=4','k=5')
acc_data[1,1]=(t.glm[1,1]+t.glm[2,2])/sum(t.glm)
acc_data[1,2]=(t.lda[1,1]+t.lda[2,2])/sum(t.lda)
acc_data[1,3]=(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)
acc_data[1,4]=(t.svm[1,1]+t.svm[2,2])/sum(t.svm)

#--------------------------------------------------------------------------------------
#2-mer
#数据预处理
neg_2mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\neg2_mer.txt')
pos_2mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\pos2_mer.txt')
neg_2mer$V17<-0
pos_2mer$V17<-1
colnames(pos_2mer)[17]='y'
colnames(neg_2mer)[17]='y'
set.seed(1)
train_num<-sample(6606,round(6606/10*9),replace = FALSE)
pos_train_data<-pos_2mer[train_num,]
pos_test_data<-pos_2mer[-train_num,]
neg_train_data<-neg_2mer[train_num,]
neg_test_data<-neg_2mer[-train_num,]
train_data<-rbind(pos_train_data,neg_train_data)
test_data<-rbind(pos_test_data,neg_test_data)

pre_data<-data.frame(test_data$y)
colnames(pre_data)<-c("y")

#logistic回归
glm.fit<-glm(y~.,data = train_data,family = binomial)
summary(glm.fit)
glm.prob<-predict(glm.fit,test_data,type='response')
glm.pred=rep(0,1322)
glm.pred[glm.prob>0.5]=1
pre_data$logistics<-glm.pred
t.glm<-table(glm.pred,test_data$y)
(t.glm[1,1]+t.glm[2,2])/sum(t.glm)#(522+481)/(522+481+139+180)#0.7586989

#线性判别回归
library(MASS)
lda.fit<-lda(y~.,data=train_data)
summary(lda.fit)
lda.pred<-predict(lda.fit,test_data[,1:16])
pre_data$lda<-lda.pred$class
t.lda<-table(lda.pred$class,test_data$y)
(t.lda[1,1]+t.lda[2,2])/sum(t.lda)#(527+475)/(527+475+186+134)#0.7579425

#随机森林模型
set.seed(2)
library('randomForest')
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优参数mtry,
n<-ncol(train_data)
rate<-1
for (i in 1:(n-1)){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
a<-which(rate==min(rate),arr.ind=TRUE)
rate[a]

#寻找最佳ntree
ntree=seq(100,1500,length=15)
rate_1<-1
for (i in 1:15){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=26,ntree=ntree[i])
  rate_1[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
b<-which(rate_1==min(rate_1),arr.ind = TRUE)
rate_1[b]

foreast.fit<-randomForest(y~.,data = train_data,mtry=32,ntree=1500)
foreast.pred<-predict(foreast.fit,test_data)
t.foreast<-table(foreast.pred,test_data$y)
(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)#(545+546)/(545+546+115+116)#0.8252648

pre_data<-cbind(pre_data,as.data.frame(foreast.pred))
colnames(pre_data)[4]<-'forest'

#svm模型
#SVM
library(e1071)
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优的gamma与cost
svm_model_tune=tune(svm,y~.,data = train_data,kernel="radial",type = "C-classification",ranges=(list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c (0.1,1,2,3,4,5,10))))
summary(svm_model_tune)
svm_fit=svm(y~.,data=train_data,cost=1,gamma=0.1,kernel="linear",type='C',probability=TRUE)
summary(svm_fit)
svm.pred=predict(svm_fit,test_data,probability = TRUE)
attr(svm.pred,"probabilities")[,1]
t.svm<-table(test_data$y,svm.pred)
(t.svm[1,1]+t.svm[2,2])/sum(t.svm)#(521+487)/(521+487+140+174)#0.7624811
pre_data<-cbind(pre_data,as.data.frame(svm.pred))
colnames(pre_data)[5]<-'svm'

pre_data$lda<-as.numeric(pre_data$lda)
pre_data$forest<-as.numeric(pre_data$forest)
pre_data$svm<-as.numeric(pre_data$svm)
pre_data[,3:5]<-pre_data[,3:5]-1
write.csv(pre_data,"C:\\Users\\87545\\Desktop\\cla_data\\2m_auc_data.csv")

auc_point_data<-pre_data
auc_point_data$logistics<-predict(glm.fit,test_data,type='response')
auc_point_data$lda<-predict(lda.fit,test_data,type='response')$posterior[,2]
auc_point_data$forest<-predict(foreast.fit,test_data,type = 'prob')[,2]
auc_point_data$svm<-attr(svm.pred,"probabilities")[,1]
write.csv(auc_point_data,"C:\\Users\\87545\\Desktop\\cla_data\\2_mer_auc_point_data.csv")

#roc计算及可视化
library(pROC)
glm.roc<-roc(auc_point_data$y,auc_point_data$logistics)#0.832
lda.roc<-roc(auc_point_data$y,auc_point_data$lda)#0.832
forest.roc<-roc(auc_point_data$y,auc_point_data$forest)#0.904
svm.roc<-roc(auc_point_data$y,auc_point_data$svm)#0.833
par(mfrow=c(1,4))
plot(glm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='Logistic')
plot(lda.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='LDA')
plot(forest.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='RandomForest')
plot(svm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='SVM')

auc_bar_data[2,1]=ROC_logistics$auc[1]
auc_bar_data[2,2]=ROC_lda$auc[1]
auc_bar_data[2,3]=ROC_rf$auc[1]
auc_bar_data[2,4]=ROC_svm$auc[1]

acc_data[2,1]=(t.glm[1,1]+t.glm[2,2])/sum(t.glm)
acc_data[2,2]=(t.lda[1,1]+t.lda[2,2])/sum(t.lda)
acc_data[2,3]=(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)
acc_data[2,4]=(t.svm[1,1]+t.svm[2,2])/sum(t.svm)
#--------------------------------------------------------------------------------------
#3-mer
#数据预处理
neg_3mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\neg3_mer.txt')
pos_3mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\pos3_mer.txt')
neg_3mer$V65<-0
pos_3mer$V65<-1
colnames(pos_3mer)[65]='y'
colnames(neg_3mer)[65]='y'
set.seed(1)
train_num<-sample(6606,round(6606/10*9),replace = FALSE)
pos_train_data<-pos_3mer[train_num,]
pos_test_data<-pos_3mer[-train_num,]
neg_train_data<-neg_3mer[train_num,]
neg_test_data<-neg_3mer[-train_num,]
train_data<-rbind(pos_train_data,neg_train_data)
test_data<-rbind(pos_test_data,neg_test_data)

pre_data<-data.frame(test_data$y)
colnames(pre_data)<-c("y")

#logistic回归
glm.fit<-glm(y~.,data = train_data,family = binomial)
summary(glm.fit)
glm.prob<-predict(glm.fit,test_data,type='response')
glm.pred=rep(0,1322)
glm.pred[glm.prob>0.5]=1
pre_data$logistics<-glm.pred
t.glm<-table(glm.pred,test_data$y)
(t.glm[1,1]+t.glm[2,2])/sum(t.glm)#(552+569)/(552+569+92+109)#0.8479576

#线性判别回归
library(MASS)
lda.fit<-lda(y~.,data=train_data)
summary(lda.fit)
lda.pred<-predict(lda.fit,test_data[,1:64])
pre_data$lda<-lda.pred$class
t.lda<-table(lda.pred$class,test_data$y)
(t.lda[1,1]+t.lda[2,2])/sum(t.lda)#(550+567)/(550+567+94+111)#0.8449319

#随机森林模型
set.seed(2)
library('randomForest')
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优参数mtry,
n<-ncol(train_data)
rate<-1
for (i in 1:(n-1)){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
a<-which(rate==min(rate),arr.ind=TRUE)
rate[a]

#寻找最佳ntree
ntree=seq(100,1500,length=15)
rate_1<-1
for (i in 1:15){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=26,ntree=ntree[i])
  rate_1[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
b<-which(rate_1==min(rate_1),arr.ind = TRUE)
rate_1[b]

foreast.fit<-randomForest(y~.,data = train_data,mtry=32,ntree=1500)
foreast.pred<-predict(foreast.fit,test_data)
t.foreast<-table(foreast.pred,test_data$y)
(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)#(590+585)/(590+585+76+71)#0.8888048

pre_data<-cbind(pre_data,as.data.frame(foreast.pred))
colnames(pre_data)[4]<-'forest'

#svm模型
#SVM
library(e1071)
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优的gamma与cost
svm_model_tune=tune(svm,y~.,data = train_data,kernel="radial",type = "C-classification",ranges=(list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c (0.1,1,2,3,4,5,10))))
summary(svm_model_tune)
svm_fit=svm(y~.,data=train_data,cost=1,gamma=0.1,kernel="linear",type='C',probability=TRUE)
summary(svm_fit)
svm.pred=predict(svm_fit,test_data,probability = TRUE)
attr(svm.pred,"probabilities")[,1]
t.svm<-table(test_data$y,svm.pred)
(t.svm[1,1]+t.svm[2,2])/sum(t.svm)#(552+569)/(552+569+109+92)#0.8479576
pre_data<-cbind(pre_data,as.data.frame(svm.pred))
colnames(pre_data)[5]<-'svm'

pre_data$lda<-as.numeric(pre_data$lda)
pre_data$forest<-as.numeric(pre_data$forest)
pre_data$svm<-as.numeric(pre_data$svm)
pre_data[,3:5]<-pre_data[,3:5]-1
write.csv(pre_data,"C:\\Users\\87545\\Desktop\\cla_data\\3m_auc_data.csv")

auc_point_data<-pre_data
auc_point_data$logistics<-predict(glm.fit,test_data,type='response')
auc_point_data$lda<-predict(lda.fit,test_data,type='response')$posterior[,2]
auc_point_data$forest<-predict(foreast.fit,test_data,type = 'prob')[,2]
auc_point_data$svm<-attr(svm.pred,"probabilities")[,1]
write.csv(auc_point_data,"C:\\Users\\87545\\Desktop\\cla_data\\3_mer_auc_point_data.csv")

#roc计算及可视化
library(pROC)
glm.roc<-roc(auc_point_data$y,auc_point_data$logistics)#0.933
lda.roc<-roc(auc_point_data$y,auc_point_data$lda)#0.933
forest.roc<-roc(auc_point_data$y,auc_point_data$forest)#0.955
svm.roc<-roc(auc_point_data$y,auc_point_data$svm)#0.933
par(mfrow=c(1,4))
plot(glm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='Logistic')
plot(lda.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='LDA')
plot(forest.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='RandomForest')
plot(svm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='SVM')

auc_bar_data[3,1]=ROC_logistics$auc[1]
auc_bar_data[3,2]=ROC_lda$auc[1]
auc_bar_data[3,3]=ROC_rf$auc[1]
auc_bar_data[3,4]=ROC_svm$auc[1]

acc_data[3,1]=(t.glm[1,1]+t.glm[2,2])/sum(t.glm)
acc_data[3,2]=(t.lda[1,1]+t.lda[2,2])/sum(t.lda)
acc_data[3,3]=(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)
acc_data[3,4]=(t.svm[1,1]+t.svm[2,2])/sum(t.svm)

#--------------------------------------------------------------------------------------
#4-mer
#数据预处理
neg_4mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\neg4_mer.txt')
pos_4mer<-read.table('C:\\Users\\87545\\Desktop\\cla_data\\pos4_mer.txt')
neg_4mer$V257<-0
pos_4mer$V257<-1
colnames(pos_4mer)[257]='y'
colnames(neg_4mer)[257]='y'
set.seed(1)
train_num<-sample(6606,round(6606/10*9),replace = FALSE)
pos_train_data<-pos_4mer[train_num,]
pos_test_data<-pos_4mer[-train_num,]
neg_train_data<-neg_4mer[train_num,]
neg_test_data<-neg_4mer[-train_num,]
train_data<-rbind(pos_train_data,neg_train_data)
test_data<-rbind(pos_test_data,neg_test_data)

pre_data<-data.frame(test_data$y)
colnames(pre_data)<-c("y")

#logistic回归
glm.fit<-glm(y~.,data = train_data,family = binomial)
summary(glm.fit)
glm.prob<-predict(glm.fit,test_data,type='response')
glm.pred=rep(0,1322)
glm.pred[glm.prob>0.5]=1
pre_data$logistics<-glm.pred
t.glm<-table(glm.pred,test_data$y)
(t.glm[1,1]+t.glm[2,2])/sum(t.glm)#(578+590)/(578+590+71+83)#0.8835098

#线性判别回归
library(MASS)
lda.fit<-lda(y~.,data=train_data)
summary(lda.fit)
lda.pred<-predict(lda.fit,test_data[,1:256])
pre_data$lda<-lda.pred$class
t.lda<-table(lda.pred$class,test_data$y)
(t.lda[1,1]+t.lda[2,2])/sum(t.lda)#(579+583)/(579+583+78+82)#0.8789713

#随机森林模型
set.seed(2)
library('randomForest')
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优参数mtry,
n<-ncol(train_data)
rate<-1
for (i in 1:(n-1)){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
a<-which(rate==min(rate),arr.ind=TRUE)
rate[a]

#寻找最佳ntree
ntree=seq(100,1500,length=15)
rate_1<-1
for (i in 1:15){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=26,ntree=ntree[i])
  rate_1[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
b<-which(rate_1==min(rate_1),arr.ind = TRUE)
rate_1[b]

foreast.fit<-randomForest(y~.,data = train_data,mtry=32,ntree=1500)
foreast.pred<-predict(foreast.fit,test_data)
t.foreast<-table(foreast.pred,test_data$y)
(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)#(590+585)/(590+585+76+71)#0.8888048

pre_data<-cbind(pre_data,as.data.frame(foreast.pred))
colnames(pre_data)[4]<-'forest'

#svm模型
#SVM
library(e1071)
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优的gamma与cost
svm_model_tune=tune(svm,y~.,data = train_data,kernel="radial",type = "C-classification",ranges=(list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c (0.1,1,2,3,4,5,10))))
summary(svm_model_tune)
svm_fit=svm(y~.,data=train_data,cost=1,gamma=0.1,kernel="linear",type='C',probability=TRUE)
summary(svm_fit)
svm.pred=predict(svm_fit,test_data,probability = TRUE)
attr(svm.pred,"probabilities")[,1]
t.svm<-table(test_data$y,svm.pred)
(t.svm[1,1]+t.svm[2,2])/sum(t.svm)#(585+594)/(583+594+67+78)#0.8903177
pre_data<-cbind(pre_data,as.data.frame(svm.pred))
colnames(pre_data)[5]<-'svm'

pre_data$lda<-as.numeric(pre_data$lda)
pre_data$forest<-as.numeric(pre_data$forest)
pre_data$svm<-as.numeric(pre_data$svm)
pre_data[,3:5]<-pre_data[,3:5]-1
write.csv(pre_data,"C:\\Users\\87545\\Desktop\\cla_data\\4m_auc_data.csv")

auc_point_data<-pre_data
auc_point_data$logistics<-predict(glm.fit,test_data,type='response')
auc_point_data$lda<-predict(lda.fit,test_data,type='response')$posterior[,2]
auc_point_data$forest<-predict(foreast.fit,test_data,type = 'prob')[,2]
auc_point_data$svm<-attr(svm.pred,"probabilities")[,1]
write.csv(auc_point_data,"C:\\Users\\87545\\Desktop\\cla_data\\4_mer_auc_point_data.csv")

#roc计算及可视化
library(pROC)
glm.roc<-roc(auc_point_data$y,auc_point_data$logistics)#0.956
lda.roc<-roc(auc_point_data$y,auc_point_data$lda)#0.957
forest.roc<-roc(auc_point_data$y,auc_point_data$forest)#0.962
svm.roc<-roc(auc_point_data$y,auc_point_data$svm)#0.957
par(mfrow=c(1,4))
plot(glm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='Logistic')
plot(lda.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='LDA')
plot(forest.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='RandomForest')
plot(svm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='SVM')

auc_bar_data[3,1]=ROC_logistics$auc[1]
auc_bar_data[3,2]=ROC_lda$auc[1]
auc_bar_data[3,3]=ROC_rf$auc[1]
auc_bar_data[3,4]=ROC_svm$auc[1]

acc_data[3,1]=(t.glm[1,1]+t.glm[2,2])/sum(t.glm)
acc_data[3,2]=(t.lda[1,1]+t.lda[2,2])/sum(t.lda)
acc_data[3,3]=(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)
acc_data[3,4]=(t.svm[1,1]+t.svm[2,2])/sum(t.svm)

#----------------------------------------------------------
#5-mer
#数据预处理
neg_5mer<-read.table('/beegfs/home/zmz/homework/01_data/classification/neg5_mer.txt')
pos_5mer<-read.table('/beegfs/home/zmz/homework/01_data/classification/pos5_mer.txt')
neg_5mer$V1025<-0
pos_5mer$V1025<-1
colnames(pos_5mer)[1025]='y'
colnames(neg_5mer)[1025]='y'
set.seed(1)
train_num<-sample(6606,round(6606/10*9),replace = FALSE)
pos_train_data<-pos_5mer[train_num,]
pos_test_data<-pos_5mer[-train_num,]
neg_train_data<-neg_5mer[train_num,]
neg_test_data<-neg_5mer[-train_num,]
train_data<-rbind(pos_train_data,neg_train_data)
test_data<-rbind(pos_test_data,neg_test_data)

pre_data<-data.frame(test_data$y)
colnames(pre_data)<-c("y")

#logistic回归
glm.fit<-glm(y~.,data = train_data,family = binomial)
summary(glm.fit)
glm.prob<-predict(glm.fit,test_data,type='response')
glm.pred=rep(0,1322)
glm.pred[glm.step.prob>0.5]=1
pre_data$logistics<-glm.pred
t.glm<-table(glm.pred,test_data$y)
(t.glm[1,1]+t.glm[2,2])/sum(t.glm)#(578+590)/(578+590+71+83)#0.8835098

#线性判别回归
library(MASS)
lda.fit<-lda(y~.,data=train_data)
summary(lda.fit)
lda.pred<-predict(lda.fit,test_data[,1:1024])
pre_data$lda<-lda.pred$class
t.lda<-table(lda.pred$class,test_data$y)
(t.lda[1,1]+t.lda[2,2])/sum(t.lda)#(604+599)/(604+599+62+57)#0.9099849


#随机森林模型
set.seed(2)
library('randomForest')
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优参数mtry,ntreee
n<-ncol(train_data)
rate<-1
for (i in 1:(n-1)){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
a<-which(rate==min(rate),arr.ind=TRUE)
rate[a]

#寻找最佳ntree
ntree=seq(100,1500,length=15)
rate_1<-1
for (i in 1:15){
  set.seed(3)
  rf_train<-randomForest(y~.,data = train_data,mtry=26,ntree=ntree[i])
  rate_1[i]<-mean(rf_train$err.rate)
  print(rf_train)
}
b<-which(rate_1==min(rate_1),arr.ind = TRUE)
rate_1[b]

foreast.fit<-randomForest(y~.,data = train_data,mtry=32,ntree=1500)
foreast.pred<-predict(foreast.fit,test_data)
t.foreast<-table(foreast.pred,test_data$y)
(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)#(582+611)/(582+611+50+79)#0.9024206

pre_data<-cbind(pre_data,as.data.frame(foreast.pred))
colnames(pre_data)[4]<-'forest'

#svm模型
#SVM
library(e1071)
train_data$y=as.factor(train_data$y)
test_data$y=as.factor(test_data$y)
#寻找最优的gamma与cost
svm_model_tune=tune(svm,y~.,data = train_data,kernel="radial",type = "C-classification",ranges=(list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c (0.1,1,2,3,4,5,10))))
summary(svm_model_tune)
svm_fit=svm(y~.,data=train_data,cost=1,gamma=0.1,kernel="linear",type="C",probability=TRUE)
summary(svm_fit)
svm.pred=predict(svm_fit,test_data,probability = TRUE)
attr(svm.pred,"probabilities")[,1]
t.svm<-table(test_data$y,svm.pred)
(t.svm[1,1]+t.svm[2,2])/sum(t.svm)#(600+592)/(600+592+61+69)#0.9016641
pre_data<-cbind(pre_data,as.data.frame(svm.pred))
colnames(pre_data)[5]<-'svm'

pre_data$lda<-as.numeric(pre_data$lda)
pre_data$forest<-as.numeric(pre_data$forest)
pre_data$svm<-as.numeric(pre_data$svm)
pre_data[,3:5]<-pre_data[,3:5]-1
write.csv(pre_data,"/beegfs/home/zmz/homework/01_data/classification/5m_auc_data.csv")

auc_point_data<-pre_data
auc_point_data$logistics<-predict(glm.fit,test_data,type='response')
auc_point_data$lda<-predict(lda.fit,test_data,type='response')$posterior[,2]
auc_point_data$forest<-predict(foreast.fit,test_data,type = 'prob')[,2]
auc_point_data$svm<-predict(svm_fit,test_data,type='prob')
auc_point_data$svm<-attr(svm.pred,"probabilities")[,1]
write.csv(auc_point_data,"/beegfs/home/zmz/homework/01_data/classification/5_mer_auc_point_data.csv")

#roc计算及可视化
glm.roc<-roc(auc_point_data$y,auc_point_data$logistics)#0.956
lda.roc<-roc(auc_point_data$y,auc_point_data$lda)#0.964
forest.roc<-roc(auc_point_data$y,auc_point_data$forest)#0.970
svm.roc<-roc(auc_point_data$y,auc_point_data$svm)#0.957
par(mfrow=c(1,4))
plot(glm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='Logistic')
plot(lda.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='LDA')
plot(forest.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='RandomForest')
plot(svm.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),max.auc.polygon=TRUE,auc.polygon.col='#b9cfed', print.thres=TRUE,main='SVM')

auc_bar_data<-array(0,c(5,4))
auc_bar_data<-as.data.frame(auc_bar_data)
colnames(auc_bar_data)<-c("logistic","lda","forest","svm")
rownames(auc_bar_data)<-c("k=1",'k=2','k=3','k=4','k=5')
auc_bar_data[5,1]=ROC_logistics$auc[1]
auc_bar_data[5,2]=ROC_lda$auc[1]
auc_bar_data[5,3]=ROC_rf$auc[1]
auc_bar_data[5,4]=ROC_svm$auc[1]

acc_data<-array(0,c(5,4))
acc_data<-as.data.frame(acc_data)
colnames(acc_data)<-c("logistic",'lda','forest','svm')
rownames(acc_data)<-c('k=1','k=2','k=3','k=4','k=5')
acc_data[4,1]=(t.glm[1,1]+t.glm[2,2])/sum(t.glm)
acc_data[4,2]=(t.lda[1,1]+t.lda[2,2])/sum(t.lda)
acc_data[4,3]=(t.foreast[1,1]+t.foreast[2,2])/sum(t.foreast)
acc_data[4,4]=(t.svm[1,1]+t.svm[2,2])/sum(t.svm)

#柱状图
acc_bar<-read.csv("/beegfs/home/zmz/homework/01_data/classification/acc_bar.csv",header = F)
ggplot(acc_bar,aes(V2,V3,fill=V1))+
  geom_bar(stat="identity",position="dodge",color='black')+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("PCC bar chart")+
  theme(axis.title = element_blank())+
  theme_bw()