HOME="E:/工作/FOHF产品/"

library(RODBC)
#连接私募数据库
simudata <- odbcConnect("zyyx_simudata", uid="select", pwd="select123"); 

ianames <- list()
fundnames <-c()

#输入起始日期
startdate <- '2015-12-15'
#输入截止日期
enddate <- '2016-12-31'

#在括号内输入产品代码，单引号的形式

fundcode <- c('30587','20441')


#读取产品净值
if(length(fundcode)!=0){
for (i in 1:length(fundcode)){
  
  sql1 <- paste('select statistic_date,swanav from t_fund_nv_data where fund_id=',fundcode[i],' and statistic_date>\'',startdate,'\' and  statistic_date<\'',enddate,'\' and swanav is not null',sep="")
  navtemp <- sqlQuery(simudata,sql1)
  names(navtemp) <- c('date','NAV')
  ianames[[i]] <- navtemp
  
  sql2 <- paste('select fund_name from t_fund_info where fund_id=',fundcode[i])
  fundtemp <- sqlQuery(simudata,sql2)
  fundname <- as.character(fundtemp[1,1])
  fundnames[i] <- fundname 
}
} else 
{ianames <- list() 

fundnames <- c() }

  
#关闭连接
odbcClose(simudata)

l <- length(ianames)

#手动导入净值

eb <- read.csv(paste(HOME,"eb-千象.csv",sep=""),sep=",",stringsAsFactors = FALSE)
eb <- na.omit(eb)
eb <- eb[which(as.Date(eb$date)>as.Date(startdate)&as.Date(eb$date)<as.Date(enddate)),]

ee <- read.csv(paste(HOME,"ee-风禾.csv",sep=""),sep=",",stringsAsFactors = FALSE)
ee <- na.omit(ee)
ee <- ee[which(as.Date(ee$date)>as.Date(startdate)&as.Date(ee$date)<as.Date(enddate)),]

bl <- read.csv(paste(HOME,"bl-九坤.csv",sep=""),sep=",",stringsAsFactors = FALSE)
bl <- na.omit(bl)
bl <- bl[which(as.Date(bl$date)>as.Date(startdate)&as.Date(bl$date)<as.Date(enddate)),]

am <- read.csv(paste(HOME,"am-盛冠达TD.csv",sep=""),sep=",",stringsAsFactors = FALSE)
am <- na.omit(am)
am <- am[which(as.Date(am$date)>as.Date(startdate)&as.Date(am$date)<as.Date(enddate)),]

an <- read.csv(paste(HOME,"an-盛世高频.csv",sep=""),sep=",",stringsAsFactors = FALSE)
an <- na.omit(an)
an <- an[which(as.Date(an$date)>as.Date(startdate)&as.Date(an$date)<as.Date(enddate)),]

fg <- read.csv(paste(HOME,"fg-数博内鑫.csv",sep=""),sep=",",stringsAsFactors = FALSE)
fg <- na.omit(fg)
fg <- fg[which(as.Date(fg$date)>as.Date(startdate)&as.Date(fg$date)<as.Date(enddate)),]

fe <- read.csv(paste(HOME,"fe-嘉理.csv",sep=""),sep=",",stringsAsFactors = FALSE)
fe <- na.omit(fe)
fe <- fe[which(as.Date(fe$date)>as.Date(startdate)&as.Date(fe$date)<as.Date(enddate)),]

fundnames <- c(fundnames,'盛冠达','数博','盛世')
#fundnames <- c('千象','盛冠达TD','盛世高频','数博内鑫','嘉理嘉翼')


ianames[[l+1]] <- am

ianames[[l+2]] <- fg

ianames[[l+3]] <- an

#ianames[[l+3]] <- an

#ianames[[l+2]] <- bl

#ianames[[l+2]] <- fe

#ianames[[l+3]] <- am

#ianames[[l+4]] <- an

#ianames[[l+5]] <- fg







