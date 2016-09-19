rm(list=ls())
library(plyr)
library(bnlearn)
library(Rgraphviz)
library(gRain)

xauusd.all<-read.csv("F:/datasets/XAUUSD1.csv",header = FALSE)
names(xauusd.all)<-c("Date","Hours","Open","High","Low","Close","Volumes")
xauusd<-xauusd.all[1:10000,]

# 计算新指标：移动均线指标MA，相邻指标NE
build_new_index<-function(x){
  x$CO<-(x$Close-x$Open)/(x$Close+x$Open)*2
  temp.CloseMinusOpen<-abs(x$Close-x$Open)
  temp.CloseMinusOpen[temp.CloseMinusOpen<0.001]<-0.001
  x$UStoCO<-(x$High-apply(X = matrix(c(x$Open,x$Close),ncol=2),MARGIN = 1,max))/temp.CloseMinusOpen
  x$DStoCO<-(apply(X = matrix(c(x$Open,x$Close),ncol=2),MARGIN = 1,min)-x$Low)/temp.CloseMinusOpen
  x$NE<-(c(NA,x$Close[-1])-x$Open)/(c(NA,x$Close[-1])+x$Open)*2
  x$MA5<-c(rep(NA,5),diff(cumsum(x$Close),5)/5)
  x$MA10<-c(rep(NA,10),diff(cumsum(x$Close),10)/10)
  x$MA30<-c(rep(NA,30),diff(cumsum(x$Close),30)/30)
  x$MA90<-c(rep(NA,90),diff(cumsum(x$Close),90)/90)
  x$MA180<-c(rep(NA,180),diff(cumsum(x$Close),180)/180)
  x$corner<-c(NA,diff(x$MA5)-diff(x$MA10))
  x$location<-x$Close-x$MA5
  return(x)
}
# 离散化指标
discrete_index<-function(x){
  y<-data.frame(CO=x$CO,UStoCO=x$UStoCO,DStoCO=x$DStoCO,NE=x$NE,corner=x$corner,location=x$location)
  #   y$CO[x$CO<=-0.03]="Bad1"
  #   y$CO[x$CO>-0.03&x$CO<=-0.00001]="Bad2"
  #   y$CO[x$CO>-0.00001&x$CO<=0.00001]="General"
  #   y$CO[x$CO>0.00001&x$CO<=0.03]="Good1"
  #   y$CO[x$CO>0.03]="Good2"
  
  y$CO[x$CO<=0]="Bad"
  y$CO[x$CO>0]="Good"
  
  y$UStoCO[x$UStoCO<=0.5]="Short"
  y$UStoCO[x$UStoCO>0.5&x$UStoCO<=1]="General"
  y$UStoCO[x$UStoCO>0.5]="Long"
  
  y$DStoCO[x$DStoCO<=0.5]="Short"
  y$DStoCO[x$DStoCO>0.5&x$UStoCO<=1]="General"
  y$DStoCO[x$DStoCO>0.5]="Long"
  
  y$NE[x$NE>0.01]="Better"
  y$NE[x$NE<=-0.01]="Worse"
  y$NE[x$NE>-0.01&x$NE<=0.01]="General"
  
  y$corner[x$corner>0]="Yes"
  y$corner[x$corner<=0]="No"
  
  y$location[x$location>0]="High"
  y$location[x$location<=0]="Low"
  return(y)
}

xauusd.addindex<-build_new_index(xauusd)
# xauusd.discrete<-as.data.frame(apply(X = discrete_index(xauusd.addindex),MARGIN = 2,function(i) factor(i)))
xauusd.discrete<-discrete_index(xauusd.addindex)

# 构造时间序列
fun.getseries.vectors<-function(vects,first,num.backpoints){#多个vect向量截取first到len=len(vect)-num.backpoints
  return(vects[first:(dim(vects)[1]-num.backpoints+first-1),])
}
backpoints<-10
data.series<-as.data.frame(t(ldply(.data = sapply(backpoints:1,function(i) fun.getseries.vectors(xauusd.discrete,i,backpoints)))))
colnames(data.series)<-paste(names(xauusd.discrete),rep(1:backpoints,each=length(names(xauusd.discrete))),sep="_")
rownames(data.series)<-c()
data.series$PreState<-as.factor(c(as.character(data.series[2:dim(data.series)[1],1]),NA))
data.series.complete<-data.series[complete.cases(data.series),]

# 构造训练集和测试集
loc.train<-sample(x=dim(data.series.complete)[1],size  =  1/10*dim(data.series.complete)[1])
train.set<-data.series.complete[loc.train,]
test.set<-data.series.complete[-loc.train,]

# 贝叶斯网络分类器
{
temp.paircode<-expand.grid(1:backpoints,1:backpoints)
pair.code<-temp.paircode[temp.paircode[,1]<temp.paircode[,2],]
pair.index<-expand.grid(names(xauusd.discrete),names(xauusd.discrete))
father.node<-as.vector(sapply(X = pair.index[,1],FUN = function(i) paste(i,pair.code[,1],sep="_")))
children.node<-as.vector(sapply(X = pair.index[,2],FUN = function(i) paste(i,pair.code[,2],sep="_")))

black.list<-data.frame(from=as.vector(father.node),to=as.vector(children.node))

bn.structure<-hc(train.set,blacklist  =  black.list)#结构训练
graphviz.plot(bn.structure,  layout  =  "fdp")
bn.parameter<-bn.fit(x  =  bn.structure,data  =  train.set)#参数学习

v.nodes<-as.vector(sapply(X = names(xauusd.discrete),FUN = function(i) paste(i,1:backpoints,sep = "_")))
trainset.jtree<-compile(as.grain(bn.parameter))
fun.bninference<-function(trainset.jtree,evidence.nodes,nodes.states,nodes.pre){
  pre.evidence<-setFinding(trainset.jtree,nodes=evidence.nodes,states = nodes.states)
  querygrain(pre.evidence,nodes=nodes.pre,type="marginal")
  
}
gc()
pre.p<-as.data.frame(matrix(unlist(apply(test.set[,-dim(test.set)[2]],MARGIN = 1,function(i) fun.bninference(trainset.jtree,v.nodes,i,"PreState"))),
                            ncol = 2,byrow = TRUE))
loc.p<-apply(pre.p==apply(X = pre.p,MARGIN = 1,FUN = max),MARGIN = 1,FUN = which)
pre.result<-factor(x = loc.p,labels = c("Bad","Good"),levels = c(1,2))
precies.df<-data.frame(real=test.set$PreState,pre=pre.result)
precise.table<-table(precies.df)
precise.table
sum(diag(precise.table))/sum(precise.table)
}

# 决策树(回归树)

{
library(rpart)
library(rpart.plot)
choose_index<-function(x){
  y<-data.frame(CO=x$CO,UStoCO=x$UStoCO,DStoCO=x$DStoCO,NE=x$NE,corner=x$corner,location=x$location,volume=x$Volumes)
  return(y)
}
xauusd.choose<-choose_index(xauusd.addindex)
backpoints<-8
data.series<-as.data.frame(t(ldply(.data = sapply(backpoints:1,function(i) fun.getseries.vectors(xauusd.choose,i,backpoints)))))
colnames(data.series)<-paste(names(xauusd.choose),rep(1:backpoints,each=length(names(xauusd.choose))),sep="_")
rownames(data.series)<-c()
data.series$PreState<-c(data.series[2:dim(data.series)[1],1],NA)
data.series.complete<-as.data.frame(scale(data.series[complete.cases(data.series),]))


loc.train<-sample(x=dim(data.series.complete)[1],size  =  1/2*dim(data.series.complete)[1])
train.set<-data.series.complete[loc.train,]
test.set<-data.series.complete[-loc.train,]

ct<-rpart.control(xval=10,minslit=50,cp=0.0077)
tfit<-rpart(formula = PreState~.,data = train.set,control = ct,method = "anova",parms = list(split="gini"))
# summary(tfit)
# par(mfrow=c(1,3))
# plot(tfit)
# text(tfit,use.n=T,all=T,cex=0.9)
# rpart.plot(tfit,branch=1,branch.type=2,type=1,extra=100,shadow.col="gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2,main="Decision Tree")
rpart.plot(tfit,type=2,faclen=T) 

pre.value<-predict(tfit,test.set[,-dim(test.set)[2]])
real.value<-test.set[,dim(test.set)[2]]
ddd<-data.frame(pre=pre.value,real=real.value)
# plot(real.value,pre.value)
cor(ddd)

sst<-sum((real.value-mean(real.value))^2)
ssr<-sum((pre.value-mean(real.value))^2)
sse<-sum((pre.value-real.value)^2)
sst
ssr
sse
plot(pre.value-real.value)
}
