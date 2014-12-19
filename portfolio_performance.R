#FX is the pnl series after taking into account foreign exchange rate since we trade at different market
FX<-read.zoo("C://Columbia MAFN/4073/Momentum PnL CSV/RETURN_FX.csv",header=TRUE,sep=',')
FXdaily<-as.xts(FX)
FXmonthly<-apply(FXdaily, MARGIN = 2,function(x) Cl(to.monthly(x)))
FXmonthly=FXmonthly+matrix(1,nrow(FXmonthly),ncol(FXmonthly))
dim(FXmonthly)

#weights are gained under Entropy Pooling method
weights<-read.csv("C://Columbia MAFN/4073/weights.csv")

FXMonthly_mod<-FXmonthly[131:174,]
write.zoo(FXMonthly,"FXMonthly.csv",sep=',')
FXM<-read.zoo("C://Columbia MAFN/4073/Momentum PnL CSV/FXMonthly.csv",header=TRUE,sep=',')
FXM_mod<-as.xts(FXM[131:174,1:52])


PortfValue=ifelse(FXM_mod[,1]>100,0,0)
PortfValue[1]<-1000000*FXM_mod[1,]%*%t(weights[1,])

for (i in 2:44) {
  
  #Portf[(1+(i-1)*44):(n+(i-1)*n)]=1000000*(FX[(1+(i-1)*n):(n+(i-1)*n)])%*%t(weights[i,])
  PortfValue[i]=PortfValue[i-1]*FXM_mod[i,]%*%t(weights[i,])
  
}
