# 下载数据到本地数据库
rm(list=ls())
library(quantmod)
library(RODBC)
con<-odbcConnect("mysql_data",uid="root",pwd="198849",DBMSencoding="utf8")
sqlQuery(con,"USE quantitative_investment")
# 1.获取当前日期的所有股票代码--如果没有其他办法先固定时间手动导到数据库中
# 1.1从数据库中读出所有的股票代码
stock_info<-sqlFetch(con,"shanghaiAshareinformation")
stock_code<-stock_info$stock_code#获取上证A股的股票代码
stock_code.symbol<-paste(stock_code,".ss",sep="")#将股票代码加上.ss

# 2. 更新所有股票数据下载至数据库
mysql.tables<-sqlQuery(con,"SHOW TABLES")

if("shanghaiasharedata" %in% mysql.tables$Tables_in_quantitative_investment){#数据库中已经有shanghaiasharedata
  sql.str<-"select DISTINCT(stockcode),max(rownames) from shanghaiasharedata GROUP BY stockcode;"#查找数据库中所有股票的最后一次更新日期
  lastupdate.infor<-sqlQuery(con,sql.str)
  stock_codes.history<-sub(".ss","",lastupdate.infor[,1],ignore.case=TRUE)
  # print("hellowold")
  for(i in 1:length(stock_code)){
    # 旧股票：根据日期是否最新确定更新方式
    if(stock_code[i] %in% stock_codes.history){
      # print("已有股票")
      loc<-which(stock_codes.history==stock_code[i])#获取该股票在历史更新中的位置
      data_start<-as.Date(lastupdate.infor[loc,2])+1#获取该股票的需要更新的开始日期
      data_end<-Sys.Date()-1#获取该股票的需要更新的结束日期
      if(data_start>=data_end){
        next # 历史时间更新和当前日期一致,无需更新
      }
      # 有股票信息时间不是最新日期的：将历史更新时间到当前时间的数据导入到数据库中
      trynext<-tryCatch({
        stock_temp<-getSymbols(as.character(stock_code.symbol[i]),src="yahoo",from=data_start,to=data_end,auto.assign=FALSE)
      },error=function(e){
        "getSymbols error"
      })
      if(length(stock_temp)==0){#有时间差，但是没有股票数据的
        print(paste(stock_code.symbol[i],"没有数据(Old Stock)"))
        next
      }
      if(trynext=="getSymbols error"){
        print(paste(stock_code.symbol[i],trynext,"访问网点出错"))
        next
      }
      df_temp<-as.data.frame(stock_temp)
      names(df_temp)<-c("Open","High","Low","Close","Volume","Adjusted")
      df_temp$StockCode<-as.character(stock_code.symbol[i])
      sqlSave(channel = con,dat = df_temp,tablename = "shanghaiasharedata",append = TRUE,rownames = TRUE)
      next
    }
    
    # 新股票：将股票数据从1990到至今的数据全部导入数据库中
    trynext<-tryCatch({
      stock_temp<-getSymbols(stock_code.symbol[i],src="yahoo",from="1990-1-1",to=Sys.Date(),auto.assign=FALSE)
    },error=function(e){
      "not found stock"
    })
    if(trynext=="not found stock"){
      print(paste(stock_code.symbol[i],"没有数据(New stock)"))
      next}
    df_temp<-as.data.frame(stock_temp)
    names(df_temp)<-c("Open","High","Low","Close","Volume","Adjusted")
    df_temp$StockCode<-stock_code.symbol[i]
    sqlSave(channel = con,dat = df_temp,tablename = "shanghaiasharedata",append = TRUE,rownames = TRUE)
  }
}else{#数据库中还没建立数据表shanghaiasharedata
  print("冷启动")
  for(i in 1:length(stock_code)){
    stock_i<-getSymbols(stock_code.symbol[i],src="yahoo",from="1990-1-1",to=Sys.Date(),auto.assign=FALSE)
    df_temp<-as.data.frame(stock_i)
    names(df_temp)<-c("Open","High","Low","Close","Volume","Adjusted")
    df_temp$StockCode<-stock_code.symbol[i]
    sqlSave(channel = con,dat = df_temp,tablename = "shanghaiasharedata",append = TRUE,rownames = TRUE)
  }
}