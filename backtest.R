HOME="E:/工作/FOHF产品/"
source(paste(HOME,'code/mutimerge1.R',sep ="" ))

names(pctseries5)

#均配
weight <- as.vector(c(rep(1/((length(pctseries5)-1)),(length(pctseries5)-1)),0))

#按次序录入子基金的权重，上面一行可以查询子基金的名称和次序，在子基金权重之后加上现金的权重，所以录入数目为子基金数量+1
#weight <- as.vector(c(0.1,0.1,0.15,0.1,0.15,0.2,0.1,0.1)) 

sum(weight)

pcttonav <- function(x){
  temp <- vector()
  
  temp[1] <- 1
  
  for (i in 1:length(x)){
    
   temp[i+1] <- temp[i]* (1+x[i]) 
    
  }
  
  return(temp)
}

nav <- rbind(rep(1,ncol(pctseries5)-1),pctseries5[,-1])

for (i in 1:ncol(nav)){
  
  nav[,i] <- pcttonav(as.numeric(pctseries5[,i+1]))
  
}

date <- as.Date(pctseries5$date)

firstdate <- date[1]-7

navdate <-as.Date(c(firstdate,date),format = "%Y%m%d")

nav <- cbind(navdate,nav,rep(1,nrow(nav)))

names(nav)[length(names(nav))] <- '现金'

navmatrix <- as.matrix(nav[,-1])

ptfnav1 <- as.data.frame(navmatrix%*%weight)

ptfnav <- cbind(as.character(nav$navdate),ptfnav1)

names(ptfnav) <- c('date','NAV')


write.csv(ptfnav,paste(HOME,'FOFnav/FOFnav1.csv',sep=""))