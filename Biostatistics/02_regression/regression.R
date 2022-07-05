#regression
#1-kmer
#读入数据
data<-read.table("C:\\Users\\87545\\Desktop\\class1_mer.txt")
y_data<-read.csv('C:\\Users\\87545\\Desktop\\y.csv')
data$y<-y_data$TPM
#划分训练集测试集
set.seed(15)
train_num<-sample(55734,50162,replace = FALSE)
train_data<-data[train_num,]
test_data<-data[-train_num,]

#岭回归
library(Matrix)
library(glmnet)
set.seed(12)
x_matrix<-model.matrix(y~.,train_data)[,-1]
y_matrix<-train_data$y
x_test_matrix<-model.matrix(y~.,test_data)[,-1]
y_test_matrix<-test_data$y
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam#1.495989
ridge.fit<-glmnet(x_matrix,y_matrix,alpha=0,lambda=bestlam)
ridge.pred<-predict(ridge.fit,s=bestlam,newx=x_test_matrix)
mean((ridge.pred-test_data$y)^2)#25661.82
cor.test(test_data$y,ridge.pred)#0.1309134

#LASSO回归
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=1)
plot(cv.out)
bestlam.lasso<-cv.out$lambda.min
lasso.fit<-glmnet(x_matrix,y_matrix,alpha = 1,lambda = bestlam.lasso)
lasso.pred<-predict(lasso.fit,s=bestlam.lasso,newx=x_test_matrix)
mean((lasso.pred-test_data$y)^2)#25661.9
cor.test(test_data$y,lasso.pred)#0.1308825

#随机森林(时间长)
library(randomForest)
foreast.fit<-randomForest(y~.,data=train_data,ntree=500,mtry=28)
foreast.pred<-predict(foreast.fit,newdata=test_data)
mean((foreast.pred-test_data$y)^2)#27992.47
cor.test(test_data$y,foreast.pred)#0.08754383

#偏最小二乘回归
library(pls)
plsr.fit<-plsr(y~.,data=train_data,scale=TRUE,validation="CV")
summary(plsr.fit)
validationplot(plsr.fit,val.type='MSEP')
plsr.pred<-predict(plsr.fit,newdata=test_data,ncomp=2)
mean((plsr.pred-test_data$y)^2)#25660.9
cor.test(test_data$y,plsr.pred)#0.1310232

#---------------------------------------------------------------
#2-kmer
#读入数据
data<-read.table("C:\\Users\\87545\\Desktop\\class2_mer.txt")
y_data<-read.csv('C:\\Users\\87545\\Desktop\\y.csv')
data$y<-y_data$TPM
#划分训练集测试集
set.seed(15)
train_num<-sample(55734,50162,replace = FALSE)
train_data<-data[train_num,]
test_data<-data[-train_num,]

#岭回归
library(glmnet)
set.seed(12)
x_matrix<-model.matrix(y~.,train_data)[,-1]
y_matrix<-train_data$y
x_test_matrix<-model.matrix(y~.,test_data)[,-1]
y_test_matrix<-test_data$y
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam#4.199509
ridge.fit<-glmnet(x_matrix,y_matrix,alpha=0,lambda=bestlam)
ridge.pred<-predict(ridge.fit,s=bestlam,newx=x_test_matrix)
mean((ridge.pred-test_data$y)^2)#25450.05
cor.test(test_data$y,ridge.pred)#0.1594602

#LASSO回归
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=1)
plot(cv.out)
bestlam.lasso<-cv.out$lambda.min
lasso.fit<-glmnet(x_matrix,y_matrix,alpha = 1,lambda = bestlam.lasso)
lasso.pred<-predict(lasso.fit,s=bestlam.lasso,newx=x_test_matrix)
mean((lasso.pred-test_data$y)^2)#25449.59
cor.test(test_data$y,lasso.pred)#0.1596352

#随机森林(时间长)
library(randomForest)
foreast.fit<-randomForest(y~.,data=train_data,ntree=1500,mtry=28)
foreast.pred<-predict(foreast.fit,newdata=test_data)
mean((foreast.pred-test_data$y)^2)#25872.53
cor.test(test_data$y,foreast.pred)#0.1855704

#偏最小二乘回归
library(pls)
plsr.fit<-plsr(y~.,data=train_data,scale=TRUE,validation="CV")
summary(plsr.fit)
validationplot(plsr.fit,val.type='MSEP')
plsr.pred<-predict(plsr.fit,newdata=test_data,ncomp=5)
mean((plsr.pred-test_data$y)^2)#25460.67
cor.test(test_data$y,plsr.pred)#0.1580569

#---------------------------------------------------------------
#3-kmer
#读入数据
data<-read.table("C:\\Users\\87545\\Desktop\\class3_mer.txt")
y_data<-read.csv('C:\\Users\\87545\\Desktop\\y.csv')
data$y<-y_data$TPM
#划分训练集测试集
set.seed(15)
train_num<-sample(55734,50162,replace = FALSE)
train_data<-data[train_num,]
test_data<-data[-train_num,]

#岭回归
library(glmnet)
set.seed(12)
x_matrix<-model.matrix(y~.,train_data)[,-1]
y_matrix<-train_data$y
x_test_matrix<-model.matrix(y~.,test_data)[,-1]
y_test_matrix<-test_data$y
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam#4.199509
ridge.fit<-glmnet(x_matrix,y_matrix,alpha=0,lambda=bestlam)
ridge.pred<-predict(ridge.fit,s=bestlam,newx=x_test_matrix)
mean((ridge.pred-test_data$y)^2)#24724.72
cor.test(test_data$y,ridge.pred)#0.2303374

#LASSO回归
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=1)
plot(cv.out)
bestlam.lasso<-cv.out$lambda.min
lasso.fit<-glmnet(x_matrix,y_matrix,alpha = 1,lambda = bestlam.lasso)
lasso.pred<-predict(lasso.fit,s=bestlam.lasso,newx=x_test_matrix)
mean((lasso.pred-test_data$y)^2)#24728.85
cor.test(test_data$y,lasso.pred)#0.2300405

#随机森林(时间长)
library(randomForest)
foreast.fit<-randomForest(y~.,data=train_data,ntree=500,mtry=28)
foreast.pred<-predict(foreast.fit,newdata=test_data)
mean((foreast.pred-test_data$y)^2)#24562.55
cor.test(test_data$y,foreast.pred)#0.2569831

#偏最小二乘回归
library(pls)
plsr.fit<-plsr(y~.,data=train_data,scale=TRUE,validation="CV")
summary(plsr.fit)
validationplot(plsr.fit,val.type='MSEP')
plsr.pred<-predict(plsr.fit,newdata=test_data,ncomp=5)
mean((plsr.pred-test_data$y)^2)#24747.33
cor.test(test_data$y,plsr.pred)#0.2283671

#--------------------------------------------------------------------------
#4-kmer
#读入数据
data<-read.table("C:\\Users\\87545\\Desktop\\class4_mer.txt")
y_data<-read.csv('C:\\Users\\87545\\Desktop\\y.csv')
data$y<-y_data$TPM
#划分训练集测试集
set.seed(15)
train_num<-sample(55734,50162,replace = FALSE)
train_data<-data[train_num,]
test_data<-data[-train_num,]

#岭回归
library(glmnet)
set.seed(12)
x_matrix<-model.matrix(y~.,train_data)[,-1]
y_matrix<-train_data$y
x_test_matrix<-model.matrix(y~.,test_data)[,-1]
y_test_matrix<-test_data$y
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam#20.07604
ridge.fit<-glmnet(x_matrix,y_matrix,alpha=0,lambda=bestlam)
ridge.pred<-predict(ridge.fit,s=bestlam,newx=x_test_matrix)
mean((ridge.pred-test_data$y)^2)#24220.34
cor.test(test_data$y,ridge.pred)#0.2689612

#LASSO回归
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=1)
plot(cv.out)
bestlam.lasso<-cv.out$lambda.min
lasso.fit<-glmnet(x_matrix,y_matrix,alpha = 1,lambda = bestlam.lasso)
lasso.pred<-predict(lasso.fit,s=bestlam.lasso,newx=x_test_matrix)
mean((lasso.pred-test_data$y)^2)#24230.2
cor.test(test_data$y,lasso.pred)#0.263658

#随机森林(时间长)
library(randomForest)
foreast.fit<-randomForest(y~.,data=train_data,ntree=500,mtry=28)
foreast.pred<-predict(foreast.fit,newdata=test_data)
mean((foreast.pred-test_data$y)^2)#24519.28
cor.test(test_data$y,foreast.pred)#0.2506512

#偏最小二乘回归
library(pls)
plsr.fit<-plsr(y~.,data=train_data,scale=TRUE,validation="CV")
summary(plsr.fit)
validationplot(plsr.fit,val.type='MSEP')
plsr.pred<-predict(plsr.fit,newdata=test_data,ncomp=7)
mean((plsr.pred-test_data$y)^2)#24245.55
cor.test(test_data$y,plsr.pred)#0.2676576

#---------------------------------------------------------------------------------------
#--------------------------------------------------------------------------
#5-kmer
#读入数据
data<-read.table("C:\\Users\\87545\\Desktop\\class4_mer.txt")
y_data<-read.csv('C:\\Users\\87545\\Desktop\\y.csv')
data$y<-y_data$TPM
#划分训练集测试集
set.seed(15)
train_num<-sample(55734,50162,replace = FALSE)
train_data<-data[train_num,]
test_data<-data[-train_num,]

#岭回归
library(glmnet)
set.seed(12)
x_matrix<-model.matrix(y~.,train_data)[,-1]
y_matrix<-train_data$y
x_test_matrix<-model.matrix(y~.,test_data)[,-1]
y_test_matrix<-test_data$y
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam#20.07604
ridge.fit<-glmnet(x_matrix,y_matrix,alpha=0,lambda=bestlam)
ridge.pred<-predict(ridge.fit,s=bestlam,newx=x_test_matrix)
mean((ridge.pred-test_data$y)^2)#24220.34
cor.test(test_data$y,ridge.pred)#0.2689612

#LASSO回归
cv.out<-cv.glmnet(x_matrix,y_matrix,alpha=1)
plot(cv.out)
bestlam.lasso<-cv.out$lambda.min
lasso.fit<-glmnet(x_matrix,y_matrix,alpha = 1,lambda = bestlam.lasso)
lasso.pred<-predict(lasso.fit,s=bestlam.lasso,newx=x_test_matrix)
mean((lasso.pred-test_data$y)^2)#24230.2
cor.test(test_data$y,lasso.pred)#0.263658

#随机森林(时间长)
library(randomForest)
foreast.fit<-randomForest(y~.,data=train_data,ntree=500,mtry=28)
foreast.pred<-predict(foreast.fit,newdata=test_data)
mean((foreast.pred-test_data$y)^2)#24673.25
cor.test(test_data$y,foreast.pred)#0.2424394

#偏最小二乘回归
library(pls)
plsr.fit<-plsr(y~.,data=train_data,scale=TRUE,validation="CV")
summary(plsr.fit)
validationplot(plsr.fit,val.type='MSEP')
plsr.pred<-predict(plsr.fit,newdata=test_data,ncomp=7)
mean((plsr.pred-test_data$y)^2)#24245.55
cor.test(test_data$y,plsr.pred)#0.2676576
#------------------------------------
pcc_data2<-cor(all_data)
write.csv(pcc_data2,'C:\\Users\\87545\\Desktop\\pcc_data.csv')
pcc_data3<-read.csv("C:\\Users\\87545\\Desktop\\pcc_data_heatmap.csv")
rownames(pcc_data3)<-pcc_data3$X
pcc_data3<-pcc_data3[,-1]
library(pheatmap)
pheatmap(pcc_data3,
         cluster_rows = FALSE,
         cluster_col = FALSE,
         display_numbers = T,
         main = "PCC heatmap")
pheatmap(pcc_data2,
         cluster_rows = FALSE,
         cluster_col = FALSE,
         display_numbers = T,
         main = "PCC heatmap",
         gaps_row = c(4,8,12,16,20),
         gaps_col = c(4,8,12,16,20))
#柱状图
pcc_bar<-read.csv("C:\\Users\\87545\\Desktop\\pcc_bar.csv",header = F)
ggplot(pcc_bar,aes(V2,V3,fill=V1))+
  geom_bar(stat="identity",position="dodge",color='black')+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("PCC bar chart")+
  theme(axis.title = element_blank())+
  theme_bw()