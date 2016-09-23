# 根据数据库中的股票数据进行选股
# 趋势追踪选股

# 滤波(排除盘整带来的波动,给定向量x和阈值threshold,计算平滑后的向量)
fun.smooth<-function(x_ts,threshold){
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

# 均线(计算移动平均线)
fun.ma<-function(x_ts,tau,na.rm=TRUE){
  y<-x_ts
  for(i in 1:(length(x_ts)-tau)){
    y[i]<-mean(x_ts[i:(i+tau-1)])
#     print(paste("x:",x_ts[i:(i+tau-1)]))
#     print(paste("y",y[i]))
  }
  return(if(na.rm) y else c(rep(NA,tau),y))
}

x<-c(1+rnorm(20,0,0.5),3+rnorm(20,0,0.5))
x
plot(x)
y<-fun.smooth(x,1.5)
plot(y)

x2<-1:10+rnorm(10,0,0.5)
x2
plot(x2)
fun.ma(x_ts = x2,tau = 2,na.rm = TRUE)
plot(y)
y<-rep(3,10)
x2
y
