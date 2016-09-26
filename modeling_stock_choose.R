# 根据数据库中的股票数据进行选股
# 趋势追踪选股
library(RODBC)
require(reshape)
{
# 滤波(排除盘整带来的波动,给定向量x和阈值threshold,计算平滑后的向量)
fun.smooth<-function(x_ts,threshold=0.1){
  y<-x_ts
  last.limit<-x_ts[1]
  loc.mark<-1
  for(i in 2:length(x_ts)){
    range<-x_ts[i]-last.limit
    if(abs(range)<threshold){
      y[loc.mark:i]<-mean(x_ts[loc.mark:i])
    }else{
      loc.mark<-i
      last.limit<-x_ts[i]
    }
  }
  return(y)
}
# test@fun.smooth
test.data<-c(rnorm(n = 8,mean = 2,sd = 0.5),rnorm(n = 10,mean = 3,sd = 0.5))
plot(test.data)
res<-fun.smooth(test.data,1.5)
plot(res)


# 均线(计算移动平均线:)
fun.ma<-function(x_ts,tau,na.rm=TRUE){
  x_ma<-0
  for(i in 1:(length(x_ts)-tau+1)){
    x_ma[i]<-mean(x_ts[i:(i+tau-1)])
  }
  return(if(na.rm) x_ma else c(rep(NA,tau-1),x_ma))
}

# 从数据库中获取股票数据
fun.get_data_from_mysql<-function(date.start="1990-01-01"){
  if(!("RODBC" %in% (.packages()))) require(RODBC)
  con<-odbcConnect("mysql_data",uid="root",pwd="198849",DBMSencoding="utf8")
  sqlQuery(con,"USE quantitative_investment")
  str_sql<-paste("SELECT * FROM shanghaiAsharedata WHERE rownames>\'",as.character(date.start),"\'",sep="")
  stock.all<-sqlQuery(con,str_sql)
  return(stock.all)
}
# test@fun.get_data_from_mysql
data.all<-fun.get_data_from_mysql('2016-06-01')

# 取每只股票的某个指标(开盘/收盘/最高/最低)数据进行分列
fun.cast_stock<-function(stock_data,var_choose="close"){
  if(!("reshape" %in% (.packages()))) require(reshape)
  data.melt<-melt(data = stock_data)
  data.cast<-cast(data = data.melt,formula = rownames~stockcode,subset = (variable==var_choose))
  return(data.cast)
}
# test@fun.cast_stock
data.cast.result<-fun.cast_stock(data.all,"open")
}

# 判断当前趋势同历史趋势的比较
fun.extreme_judege<-function(x_ts,drift=0){
  res_trend<-x_ts
  res_trend[]<-0
  for(i in 2:length(x_ts)){
    trend<-x_ts[i]-x_ts[i-1]
    count<-0
    for(j in i-1:1){
      if(x_ts[j]==x_ts[i]){
        count<-count+1
      }else{
        break
      }
    }
    if(trend<0-drift*count){
      res_trend[i]<--1
    }
    if(trend>0+drift*count){
      res_trend[i]<-1
    }
  }
  
  res_signal<-res_trend
  res_signal[]<-"L"
#   res_signal[res_trend==0]<-"look"
#   res_signal[res_trend==1]<-"buy"
#   res_signal[res_trend==-1]<-"sell"
  for(i in 2:(length(res_signal)-1)){
    if(res_trend[i]==-1&res_trend[i-1]==-1){
      res_signal[i]<-"S"
    }
    if(res_trend[i]==1&res_trend[i-1]==1){
      res_signal[i]<-"B"
    }
  }

  
  res_ext<-x_ts
  if(x_ts[1]<x_ts[2]) res_ext[1]<-"min"
  if(x_ts[1]>x_ts[2]) res_ext[1]<-"max"
  if(x_ts[1]==x_ts[2]) res_ext[1]<-"medium"
  for(i in 2:(length(x_ts)-1)){
    if(x_ts[i]<x_ts[i-1] & x_ts[i]<x_ts[i+1]){
      res_ext[i]="min"
      next
    }
    if(x_ts[i]>x_ts[i-1] & x_ts[i]>=x_ts[i+1]){
      res_ext[i]="max"
      next
    }
    res_ext[i]="Unknown"
  }
  if(x_ts[length(x_ts)]<x_ts[length(x_ts)-1]) res_ext[length(x_ts)]<-"min"
  if(x_ts[length(x_ts)]>x_ts[length(x_ts)-1]) res_ext[length(x_ts)]<-"max"
  if(x_ts[length(x_ts)]==x_ts[length(x_ts)-1]) res_ext[length(x_ts)]<-res_ext[length(x_ts)-1]
  
  res_smooth<-x_ts
  res_smooth<-fun.smooth(x_ts)
  
  return(data.frame(ma=x_ts,extreme=res_ext,smooth=res_smooth,trend=res_trend,signal=res_signal))
}
# test@fun.extrem_judege
real.data<-fun.cast_stock(fun.get_data_from_mysql(date.start = "2016-05-01"),"close")[,2]
ma.stock<-fun.ma(real.data,tau=10,na.rm = TRUE)
# ma.stock<-apply(X = cast.stock[,-1],MARGIN = 2,FUN = function(i){fun.ma(i,tau = 10,na.rm = FALSE)})

res<-fun.extreme_judege(ma.stock)
plot(ma.stock)
lines(real.data[-c(1:9)],col="blue")
lines(res$ma,col="red")
text(1:96,res$ma+0.2,res$signal)




# 给定一只股票数据，确定该股票是否该买入
fun.strategy<-function(stock_data,method="strategy1"){
  if(method=="strategy1"){
    res<-fun.strategy1(stock_data)
  }else{
    res<-"take a look"
  }
}
fun.strategy1<-function(stock_data,tau_num){
  # 采用均线趋势策略
  ma.data<-fun.ma(x_ts = stock_data,tau = tau_num,na.rm = TRUE)#求均线
  smooth.data<-fun.smooth(x_ts = ma.data,threshold = 0.1)#平滑均线
  extreme.value<-fun.extreme_judege(smooth.data)
  
}

# 运行函数入口
fun.main<-function(){
  # 取得所有股票近期数据
  stock.data<-fun.get_data_from_mysql(date.start = "2016-05-01")#从数据库中取数据
  cast.stock<-fun.cast_stock(stock.data,"close")
  # ma.stock<-apply(X = cast.stock[,-1],MARGIN = 2,FUN = function(i){fun.ma(i,tau = 10,na.rm = FALSE)})
  ma.stock<-data.frame(date=cast.stock[-c(1:9),1],ma=fun.ma(x_ts = cast.stock[,2],tau = 10,na.rm = TRUE))

  smooth.stock<-data.frame(date=ma.stock$date,smooth=fun.smooth(x_ts = ma.stock[,2],threshold = 0.1))
  plot(ma.stock)
  lines(smooth.stock,col="red")
  lines(cast.stock[-c(1:9),1:2],col="blue")
  # 通过计算股票的历史数据确定每只股票的买卖信号
}





