# funtion files

# 从数据库中获取某只股票数据
fun.get_data_from_mysql<-function(date.start="1990-01-01",stock.code="600000.ss"){
  if(!("RODBC" %in% (.packages()))) require(RODBC)
  con<-odbcConnect("mysql_data",uid="root",pwd="198849",DBMSencoding="utf8")
  sqlQuery(con,"USE quantitative_investment")
  str_sql<-paste("SELECT * FROM shanghaiAsharedata WHERE rownames>\'",as.character(date.start),"\'","and stockcode=\'",stock.code,"\'",sep="")
  stock.all<-sqlQuery(con,str_sql)
  return(stock.all)
}

# 从数据库中获取从某个时间开始的所有股票数据
fun_get_datas_from_mysql<-function(date.start="1990-01-01"){
  if(!("RODBC" %in% (.packages()))) require(RODBC)
  con<-odbcConnect("mysql_data",uid="root",pwd="198849",DBMSencoding="utf8")
  sqlQuery(con,"USE quantitative_investment")
  str_sql<-paste("SELECT * FROM shanghaiAsharedata WHERE rownames>\'",as.character(date.start),"\'",sep="")
  stock.all<-sqlQuery(con,str_sql)
  return(stock.all)
}

# 返回数据上涨的起点
fun_find_rise<-function(x_ts,rise_percentage=0.2,time_max=8){
  points<-c()
  for(i in 1:(length(x_ts)-time_max)){
    for(j in 1:time_max){
      if(x_ts[i+j]/x_ts[i]>=rise_percentage+1){
        points<-c(points,i)
        break
      }
    }
  }
  return(points)
}

# 返回数据上涨的起点
fun_find_fall<-function(x_ts,fall_percentage=0.2,time_max=8){
  points<-c()
  for(i in 1:(length(x_ts)-time_max)){
    for(j in 1:time_max){
      if(x_ts[i+j]/x_ts[i]<=1-fall_percentage){
        points<-c(points,i)
        break
      }
    }
  }
  return(points)
}

# 计算给定点的MA值
fun_cal_ma<-function(x_ts,point,tao=5){
  if(point<tao){
    res<-mean(x_ts[1:point])
  }else{
    res<-mean(x_ts[(point-tao+1):point])
  }
  return(res)
}
fun_cal_mas<-function(x_ts,points,tao=5){
  return(sapply(X = points,FUN = function(i) fun_cal_ma(x_ts,i,tao)))
}

# 计算给定股票的大涨和大跌两种情况的指标DMA(ma(short)-ma(long))是否有显著差别
fun_cal_p_stock<-function(stock_code,tau_short_max=30,tau_long_max=200){
  stock_data<-fun.get_data_from_mysql(stock.code = stock_code)
  stock_data_close<-stock_data$close
  df<-data.frame(short=c(),long=c(),vartest=c(),ttest=c())
  points_rise<-fun_find_rise(stock_data_close)
  points_fall<-fun_find_fall(stock_data_close)
  for(i in 1:tau_short_max){
    for(j in (i+5):tau_long_max){
      d1<-fun_cal_mas(x_ts = stock_data_close,points = points_rise,tao=i)-fun_cal_mas(x_ts = stock_data_close,points = points_rise,tao=j)
      d2<-fun_cal_mas(x_ts = stock_data_close,points = points_fall,tao=i)-fun_cal_mas(x_ts = stock_data_close,points = points_fall,tao=j)
      var_test_res<-var.test(d1,d2)$p.value
      t_test_res<-t.test(d1,d2)$p.value
      df<-rbind(df,data.frame(short=i,long=j,vartest=var_test_res,ttest=t_test_res))
    }
  }
  return(df)
}

# 计算给定序列，大涨和大跌两种情况下的指标DMA(ma(short)-ma(long))是否有显著差别
fun_cal_p_ts<-function(stockdata_ts,tau_short_max=30,tau_long_max=200){
  df<-data.frame(short=c(),long=c(),vartest=c(),ttest=c())
  points_rise<-fun_find_rise(stockdata_ts)
  points_fall<-fun_find_fall(stockdata_ts)
  for(i in 1:tau_short_max){
    for(j in (i+5):tau_long_max){
      d1<-fun_cal_mas(x_ts = stockdata_ts,points = points_rise,tao=i)-fun_cal_mas(x_ts = stockdata_ts,points = points_rise,tao=j)
      d2<-fun_cal_mas(x_ts = stockdata_ts,points = points_fall,tao=i)-fun_cal_mas(x_ts = stockdata_ts,points = points_fall,tao=j)
      var_test_res<-var.test(d1,d2)$p.value
      t_test_res<-t.test(d1,d2)$p.value
      df<-rbind(df,data.frame(short=i,long=j,vartest=var_test_res,ttest=t_test_res))
    }
  }
  return(df)
}

# 计算所有股票的最有显著差别的周期值
fun_cal_pmin<-function(){
  all_stock_data<-fun_get_datas_from_mysql()
  stock_codes<-levels(all_stock_data$stockcode)
  print("1")
  df_res<-data.frame(short=c(),long=c(),vartest=c(),ttest=c(),stockcode=c())
  for(i in 1:length(stock_codes)){
    print("2")
    ts_close<-all_stock_data[all_stock_data$stockcode==stock_codes[i],c("close")]
    print("3")
    if(length(ts_close)<600) break
    t1<-Sys.time()
    df_p<-fun_cal_p_ts(ts_close)
    t2<-Sys.time()
    df_min<-df_p[df_p$ttest==min(df_p$ttest),]
    df_res<-rbind(df_res,cbind(df_min,stockcode=stock_codes[i]))
    t3<-Sys.time()
    print(paste("fun_cal_p_ts cost time: ",t2-t1))
    print(paste("others cost time: ",t3-t2))
    print(i)
  }
  return(df_res)
}


