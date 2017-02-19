setwd("E://Rweibo")
data<-read.csv("test217.csv",header=TRUE,sep=",",row.names="movie")
names(data)
data<-within(data,{
	boxoffice<-as.numeric(boxoffice)
	number<-as.numeric(number)
	class1<-as.factor(class1)
	location<-as.factor(location)
	series<-as.factor(series)
	success<-as.factor(success)
	fir<-as.factor(fir)})
attach(data)
head(data)
before<-data.frame(class1,location,series,fir,success)
head(before)

#Before: 即电影上映之前 

set.seed(1234)
train<-sample(nrow(before),nrow(data)*0.8)
train.be<-before[train,]
test.be<-before[-train,]
test.res<-before[-train,"success"]

#desTree
attach(before)
library(tree)
dtree<-tree(success~.,data=train.be)
summary(dtree)
plot(dtree)
text(dtree,pretty=0)

dtree.pred<-predict(dtree,test.be,type="class")
table(dtree.pred,test.res,dnn=c("PRED","ACTU"))
mean(dtree.pred == test.res)  #0.6538462

#剪枝
set.seed(3)
cv.dtree<-cv.tree(dtree,FUN=prune.misclass)
cv.dtree
plot(cv.dtree$size,cv.dtree$dev,type="b")

prune<-prune.misclass(dtree,best=2)
plot(prune)
text(prune,pretty=0)
prune.pred<-predict(prune,test.be,type="class")
mean( prune.pred == test.res)  # 只剩两支

#logistic
fit.glm<-glm(success~.,data=train.be,family=binomial())
summary(fit.glm)

prob<-predict(fit.glm,test.be,type="response")
prob
logit.pred<-factor(prob >.75,levels=c(FALSE,TRUE),labels=c(0,1))
table(logit.pred,test.res)
mean(logit.pred == test.res)   #0.7115385
 


#rf
library(randomForest)
rf<-randomForest(success~.,train.be,importance=TRUE,ntree=1500)
rf

varImpPlot(rf) #series>location>fir>class1

rf.pred<-predict(rf,test.be)
table(rf.pred,test.res)
mean(rf.pred == test.res)   #0.6346154

#svm
library(e1071)
fit.svm<-svm(success~.,train.be)
fit.svm
svm.pred<-predict(fit.svm,test.be)

table(svm.pred,test.res,dnn=c("PRED","ACTU"))
mean(svm.pred == test.res)   # 0.7307692

###############################################################################
#knn
library(class)
knndata<-within(data,{
	boxoffice<-scale(boxoffice)
	number<-scale(number)
	ratings<-scale(ratings)})
names(knndata)
knndata<-knndata[,-1]


set.seed(1234)
train<-sample(nrow(knndata),nrow(knndata)*0.8)
train.be<-knndata[train,-7]
test.be<-knndata[-train,-7]
train.res<-knndata[-train,"success"]
test.res<-knndata[-train,"success"]
test.be
test.res

sum(is.na(test.be))
#
缺失值
test.be<-na.omit(test.be)
test.res<-na.omit(test.res)
knn.pred<-knn(train.be,test.be,train.res,k=3)

dim(train.be)
dim(train.res)
#############################################################################
setwd("E://Rweibo")
data<-read.csv("test217.csv",header=TRUE,sep=",",row.names="movie")
names(data)
data<-within(data,{
	boxoffice<-as.numeric(boxoffice)
	number<-as.numeric(number)
	emo<-ratings*number
	class1<-as.factor(class1)
	location<-as.factor(location)
	series<-as.factor(series)
	success<-as.factor(success)
	fir<-as.factor(fir)})

sum(is.na(data))
data<-na.omit(data)

attach(data)
data1<-data[boxoffice<17400,]
dim(data1)

attach(data1)
data2<-data1[number<17000,]

dim(data2)
attach(data2)

par(mfrow=c(1,2))
plot(ratings,boxoffice,xlab="豆瓣评分",ylab="票房",font.lab=2,font=2,col="blue")
plot(number,boxoffice,xlab="影评数量",ylab="票房",font.lab=2,font=2,col="blue")


data2<-within(data2,{
	boxoffice<-scale(boxoffice)})

attach(data2)

#处理ratingsra
#ratingslims
ratingslims<-range(data2$ratings)
ratings.grid<-seq(from=ratingslims[1],to=ratingslims[2],0.1)

#splines
library(splines)
fit<-lm(boxoffice~bs(ratings,knots=c(4.1,5.8,7.5)),data=data2)
pred<-predict(fit,newdata=list(ratings=ratings.grid),se=T)
plot(ratings,boxoffice,col="gray")
lines(ratings.grid,pred$fit,lwd=2,col="blue")

fit2<-lm(boxoffice~ns(ratings,df=6),data=data2)
fit3<-lm(boxoffice~ns(ratings,df=3),data=data2)
pred2<-predict(fit2,newdata=list(ratings=ratings.grid),se=TRUE)
pred3<-predict(fit3,newdata=list(ratings=ratings.grid),se=TRUE)

plot(ratings,boxoffice,col="gray",xlab="豆瓣评分",ylab="票房",main="Natural Splines",font.lab=2)
lines(ratings.grid,pred$fit,lwd=2,col="red",lty=1)
lines(ratings.grid,pred2$fit,lwd=2,col="blue",lty=2)
lines(ratings.grid,pred3$fit,lwd=2,lty=3) #df=6 比较好
legend("topleft",inset=.05,title="自由度",c("df=3","df=6","df=9"),lty=c(3,2,1),col=c("black","blue","red"))


#选择合适的df
plot(ratings,boxoffice,xlim=ratingslims,cex=.5,col="darkgrey")
title("RAINSTOP")
fit<-smooth.spline(ratings,boxoffice,df=5)
fit2<-smooth.spline(ratings,boxoffice,cv=TRUE)
fit2$df # 最佳df=5 
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)


#处理number
plot(number,boxoffice,cex=.5,col="darkgrey",xlab="影评数量",ylab="票房",main="Natural Splines",font.lab=2)
)
title("RAINSTOP")
fit<-smooth.spline(number,boxoffice,cv=T)
fit$df # 最佳df=3
fit2<-smooth.spline(number,boxoffice,df=5)
fit3<-smooth.spline(number,boxoffice,df=7)
lines(fit,col="red",lwd=2,lty=1)
lines(fit2,col="blue",lwd=2,lty=2)
lines(fit3,col="black",lwd=2,lty=3)
legend("topleft",inset=.05,title="自由度",c("df=3","df=5","df=7"),lty=c(1,2,3),col=c("red","blue","black"))


#训练集、测试集 
set.seed(1234)
train<-sample(nrow(data2),nrow(data2)*0.8)
train.data2<-data2[train,]
test.data2<-data2[-train,]
test.res<-data2[-train,"boxoffice"]

#GAM
library(gam)

gam.m6<-gam(boxoffice~s(ratings,6)+s(number,3)+class1+location+series+success,data=train.data2)
pred.m6<-predict(gam.m6,test.data2)

#test err by m6
mean((test.res-pred.m6)^2) # MSE=0.5200413
plot(test.res,pred.m6)
abline(0,1)

##ns
ns.m6<-gam(boxoffice~ns(ratings,6)+ns(number,3)+class1+location+series+success,data=train.data2)
pred.ns6<-predict(ns.m6,test.data2)
mean((test.res-pred.ns6)^2)   #MSE=0.633439
plot(test.res,pred.ns6)
abline(0,1)
summary(ns.m6)

ns.m4<-gam(boxoffice~ns(ratings,6)+ns(number,3)+class1+success,data=train.data2)
pred.ns4<-predict(ns.m4,test.data2)
summary(ns.m4)
mean((test.res-pred.ns4)^2)
plot(test.res,pred.ns4)
abline(0,1)
##locals
lo.m6<-gam(boxoffice~lo(ratings,span=.8)+lo(number,span=.8)+class1+location+series+success,data=train.data2)
pred.lo6<-predict(lo.m6,test.data2)
mean((test.res-pred.lo6)^2) #1542854
plot(test.res,pred.lo6)
abline(0,1)


#最总版作图
#GAM
library(gam)

gam.m6<-gam(boxoffice~s(ratings,16)+s(number,16)+class1+location+series+success,data=data2)

pred.m6<-predict(gam.m6,data2)
length(pred.m6)
mean((data2$boxoffice-pred.m6)^2) # 1736265 

x1<-data2$boxoffice[1:30,]
length(x1)
x2<-pred.m6[1:30,]
plot(data2$boxoffice,pred.m6,xlab="实际票房",ylab="预测票房",main="预测结果",cex=.5,col="darkgrey",font.lab=2)
abline(0,1,lwd=4,col="red")

plot(x1,pred.m6[1:30])
abline(0,1)







