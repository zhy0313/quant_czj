# 基于策略d的选股
# d.基于均线的选股策略
# 当前股价>10日均线&当前股价>5日均线->good
# 当前股价<10日均线,5日均线<10日均线->bad
# 股票高位：在历史股价tau中处于分布的高端gamma区间
# 股票低位：在历史股价tau中处于分布的低端gamma区间
# 高位+bad->止损
# 低位+good->买股
rm(list=ls())
source(file = "czjfunset.R")

fun_stock_mastate<-function(x_ts){
  num<-length(x_ts)
  if(num>10){
    res=0
    ma5<-fun_cal_ma(x_ts = x_ts,point = num,tao = 5)
    ma10<-fun_cal_ma(x_ts = x_ts,point = num,tao = 5)
    price_now<-x_ts[num]
    if(price_now>ma5&price_now>ma10) res<-(price_now-ma5)+(price_now-ma10)
    if(price_now<ma10&price_now<ma10) res<-(price_now-ma5)+(price_now-ma10)
  }else{
    res=NA
  }
  return(res)
}
fun_stock_locstate<-function(x_ts,tau=100,gamma_up=0.8,gamma_down=0.2){
  res<-c()
  num<-length(x_ts)
  threa_up<-quantile(x_ts,gamma_up)
  threa_down<-quantile(x_ts,gamma_down)
  if(num<tau){
    res<-NA
  }else{
    res<-0
    if(x_ts[num]>threa_up) res<-x_ts[num]-threa_up
    if(x_ts[num]<threa_down) res<-x_ts[num]-threa_down
  }
  return(res)
}


stockdata<-fun_get_datas_from_mysql(date.start = as.character(Sys.Date()-300))

stockcode<-levels(stockdata$stockcode)
df<-data.frame(stock=c(),mastate=c(),locstate=c())
for(i in 1:length(stockcode)){
  stock_temp<-stockdata[stockdata$stockcode==stockcode[i],c("close")]
  stock_mastate<-fun_stock_mastate(stock_temp)
  stock_locstate<-fun_stock_locstate(stock_temp)
  df<-rbind(df,data.frame(stock=stockcode[i],mastate=stock_mastate,locstate=stock_locstate))
}
df_order<-df[order(df$mastate,decreasing = TRUE),]
