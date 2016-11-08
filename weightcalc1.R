HOME="E:/����/FOHF��Ʒ/"
source(paste(HOME,'code/mutimerge1.R',sep ="" ))

library(quadprog)
library(fPortfolio)

ptf <- as.timeSeries(pctseries5)

ptffrontier <- portfolioFrontier(ptf)

#portres <- frontier@portfolio

ptfweight <- as.data.frame(getWeights(ptffrontier))

ptfret1 <- as.data.frame(getTargetReturn(ptffrontier))

ptfret <- as.data.frame((1+ptfret1$mean)^52-1)

ptfvol1 <- as.data.frame(getTargetRisk(ptffrontier))

ptfvol <- as.data.frame(ptfvol1$Cov*sqrt(52))

ptfsharpe <- as.data.frame((ptfret-0.02)/ptfvol)

ptfout <- cbind(ptfweight,ptfret,ptfvol,ptfsharpe)

names(ptfout)<- c(fundnames,"Ŀ���껯����","Ŀ���껯����","���ձ�")

write.csv(ptfout,paste(HOME,'FOFweight/FOFweight15.csv',sep=""))