HOME="E:/����/FOHF��Ʒ/"
source(paste(HOME,'code/datainput.R',sep ="" ))

#����ת��Ϊ��
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

#�ҵ�һ������,�����ʽ"201402"
firstfri <- function(x){
  
  year <- substr(x,1,4)
  firstday <- as.Date(paste(substr(x,1,4),"01","01",sep="-"))
  weekday1 <- weekdays(firstday)
  if(weekday1 =="����һ") return(as.Date.numeric(4,firstday))
  if(weekday1 =="���ڶ�") return(as.Date.numeric(3,firstday))
  if(weekday1 =="������") return(as.Date.numeric(2,firstday))
  if(weekday1 =="������") return(as.Date.numeric(1,firstday))
  if(weekday1 =="������") return(as.Date.numeric(0,firstday))
  if(weekday1 =="������") return(as.Date.numeric(6,firstday))
  if(weekday1 =="������") return(as.Date.numeric(5,firstday))
  
}

weektofri <- function(x){
  
  startday <- firstfri(x) 
  outday <- as.Date.numeric(7*as.numeric(substr(x,5,6)),startday)
  return(outday)
}

numtodate <- function(x){
  
  x1 <- as.character(as.Date.numeric(x,"1970-01-01"))
  return(x1)
  
}

#������Ƶ�ʵľ�ֵת��Ϊ��Ƶ�ʵľ�ֵ
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

#�����ܾ�ֵ������
weekNAVpct <- function(x){
  
  for(i in 2:nrow(x)){
    
    pct <- round(as.numeric(x$NAV[i])/as.numeric(x$NAV[i-1])-1,digits=8)
    x[i,4] <- pct
  }
  res <- x[-1,3:4]
  names(res) <- c('week','pct')
  return(res)
}

#�������ֵ����������
xNAVpct <- function(x){
  
  x1 <- datetoweek(x)
  x2 <- weekNAV(x1)
  x3 <- weekNAVpct(x2)
  
  return(x3)
}


#�ϲ��������ݺ���
multimerge<-function(dat=list()){
  if(length(dat)<2)return(as.data.frame(dat))
  mergedat<-dat[[1]]
  dat[[1]]<-NULL
  for(i in dat){
    mergedat<-merge(mergedat,i,by="week",all = TRUE)
  }
  return(mergedat)
}


#��ֵת��������������
outpct <- function(dat=list()){
  
  iapct <- list()
  for(i in 1:length(dat)){
    pcttemp <- xNAVpct(as.data.frame(dat[i]))
    iapct[[i]] <- pcttemp
  }
return(iapct)

}


#������в�Ʒ���������ڶ���ı�
iapct <- outpct(ianames)
pctseries <- multimerge(iapct)
names(pctseries) <- c('week',fundnames)

#��ת��Ϊ�����������
week <- as.data.frame(pctseries[,1])

week2 <- data.frame()

for(i in 1:nrow(week)){
  
  week2[i,1] <- weektofri(week[i,1])
}

#week2 <- apply(week, 2, weektofri)

pctseries3 <- cbind(week2,pctseries[,-1])

names(pctseries3) <- c('date',fundnames)

#��������ʱ������
alldate <- as.data.frame(seq(from=pctseries3$date[1],to=pctseries3$date[nrow(pctseries3)],by=7))
names(alldate) <- c("date")
pctseries4 <- merge(alldate,pctseries3,by="date",all.x=TRUE)
date3 <- as.data.frame(pctseries4$date)
date4 <- apply(date3,2,numtodate)
pctseries5 <- cbind(date4,pctseries4[,-1])
pctseries5[is.na(pctseries5)] <- 0
names(pctseries5) <- c('date',fundnames)



write.csv(pctseries5,paste(HOME,'FOFres/FOFres13.csv',sep=""))

