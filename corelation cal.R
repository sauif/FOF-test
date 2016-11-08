HOME="E:/工作/FOHF产品/"
source(paste(HOME,'code/datainput.R',sep ="" ))
##日期转化为周
datetoweek <- function(x)
{
  datechr <- as.character(x$date)
  for(i in 1:nrow(x)){
  firstdate1 <- as.Date(paste(substr(as.character(datechr[i]),1,4),"01","01",sep="-"))
  datediff <- as.numeric(as.Date(datechr[i])-firstdate1)
  weekofyear1 <- ceiling(datediff/7)
  if(weekofyear1<10)
  {
    week1 <-paste(substr(as.character(datechr[i]),1,4),"0",as.character(weekofyear1),sep="")
  }
  else
  {
    week1 <-paste(substr(as.character(datechr[i]),1,4),as.character(weekofyear1),sep="")
  }
  
  x[i,3] <- week1
  }
  names(x) <- c("date","NAV","week")
  return(x)
  
}

##超过周频率的净值转换为周频率的净值
weekNAV <- function(x){
  y <- data.frame(stringsAsFactors = FALSE)
  li <- by(x,x$week,function(xx){
    
    if (nrow(xx)==1){
    y <<- rbind(y,xx)
    }
    else{
      maxd <- max(xx$date)
      maxxx <- xx[which(xx$date==maxd),]
      y <<- rbind(y,maxxx)
      
    }
    
  })
  return(y)
  
}

#计算周净值增长率
weekNAVpct <- function(x){
  
  for(i in 2:nrow(x)){
    
    pct <- round(as.numeric(x$NAV[i])/as.numeric(x$NAV[i-1])-1,digits=8)
    x[i,4] <- pct
    }
  res <- x[-1,3:4]
  res <- na.omit(res)
  names(res) <- c('week','pct')
  return(res)
}

#计算两个产品的相关性：调整为周净值，相同时间区间
corpr <-function(x,y){
  
  xweek <- datetoweek(x)
  yweek <- datetoweek(y)
  
  xweekNAV <- weekNAV(xweek)
  yweekNAV <- weekNAV(yweek)
  
  xNAVpct <- weekNAVpct(xweekNAV) 
  yNAVpct <- weekNAVpct(yweekNAV)
  
  xym <- merge(xNAVpct,yNAVpct,by="week")
  
  return(cor(as.numeric(xym$pct.x),as.numeric(xym$pct.y)))
  
}

#输出两个产品相同区间时间序列
mergepr <-function(x,y){
  
  xweek <- datetoweek(x)
  yweek <- datetoweek(y)
  
  xweekNAV <- weekNAV(xweek)
  yweekNAV <- weekNAV(yweek)
  
  xNAVpct <- weekNAVpct(xweekNAV) 
  yNAVpct <- weekNAVpct(yweekNAV)
  
  xym <- merge(xNAVpct,yNAVpct,by="week")
  
  return(xym)
  
}


#输出相关系数矩阵
cmatrix <- data.frame()
for (i in 1:length(ianames))
{
  for (j in 1:length(ianames)){
    
    cmatrix[i,j] <- corpr(ianames[[i]],ianames[[j]])

  }

}


names(cmatrix) <- fundnames


write.csv(cmatrix,paste(HOME,'corres/correlation-zh56.csv',sep=""))






