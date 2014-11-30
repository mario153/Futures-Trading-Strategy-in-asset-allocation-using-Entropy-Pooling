library(quantmod)
library(TTR)
library(plyr)
library(ggplot2)
library(scales)
library(PerformanceAnalytics)


BuySellSig<-function(cdata){
    mv5=na.omit(SMA(cdata,5))
    mv14=na.omit(SMA(cdata,14))
    mv20=na.omit(SMA(cdata,20))
    EMA12=na.omit(EMA(cdata,12))
    EMA26=na.omit(EMA(cdata,26))
    sig0=ifelse(cdata>0,0,0)
    sig1=ifelse(cdata>mv5,0,0)
    s1=as.numeric(ifelse(cdata>mv5,1,-1))
    for(i in 2:length(s1)){
      dif1 <- s1[i] - s1[i-1]
      if(dif1>0){
        sig1[i]=1
      }
      else if(dif1<0){
        sig1[i]=-1
      }
      else{
        sig1[i]=0
      }
    }
    
    diff1=mv14-mv20
    sig2=ifelse(diff1>0,0,0)
    s2=as.numeric(ifelse(diff1>0,1,-1))
    for(i in 2:length(s2)){
      dif2 <- s2[i] - s2[i-1]
      if(dif2>0){
        sig2[i]=1
      }
      else if(dif2<0){
        sig2[i]=-1
      }
      else{
        sig2[i]=0
      }
    }
    
    MACD=EMA12-EMA26
    EMA9=na.omit(EMA(MACD),9)
    diff2=MACD-EMA9
    sig3=ifelse(diff2>=0,0,0)
    s3=as.numeric(ifelse(diff2>0,1,-1))
    for(i in 2:length(s3)){
      dif3 <- s3[i] - s3[i-1]
      if(dif3>0){
        sig3[i]=1
      }
      else if(dif3<0){
        sig3[i]=-1
      }
      else{
        sig3[i]=0
      }
    }
    sig0=sig1+sig2+sig3
    #for(i in 1:length(sig0)){
     # if(sig0[i]==2){
      #  sig0[i]=1
      #}
      #else if(sig0[i]==-2){
       # sig0[i]=-1
      #}
    #}
    return (sig0)     
}



download<-function(stock,from="1999-00-08"){
  df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  #download data
  write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) }
read<-function(stock){  
  +  as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
}
stock<-"IBM"
download(stock,from='1999-09-08')
IBM<-read(stock)
class(IBM)
head(IBM)

odata<-IBM
cdata<-IBM$IBM.Close

sig<-BuySellSig(IBM$IBM.Close)
head(sig)
ret=na.omit(ROC(type='discrete',IBM$IBM.Close)*sig)
eq=cumprod(1+ret)
plot(eq)
table.Drawdowns(ret,top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)

###trading
capital = 100000
tdata<-sig * cdata
ATR=ATR(odata)$atr[35:length(cdata)]
BB=BBands(cdata)[35:length(cdata)]
BBup=BB$up
BBdn=BB$dn

#trade function
#tdata is price data multiply by signal
#capital is $$$ hold on hand

trade <- function(cdata, tdata, sig, capital, position = 1, fee = 0){
  sig.num <-as.numeric(sig)
  cdata.num <- as.numeric(cdata[35:length(cdata)])
  tdata.num <- as.numeric(tdata)
  ATR.num <- as.numeric(ATR)
  BBUP.num <- as.numeric(BBup)
  BBDN.num <- as.numeric(BBdn)
  positionSize <- ifelse(tdata.num==0,0,0)
  amtBuy <- ifelse(tdata.num==0,0,0)
  amtSell <- ifelse(tdata.num==0,0,0)
  tBuy <- ifelse(tdata.num==0,0,0)
  pSell <- ifelse(tdata.num==0,0,0)#percentage of shares to sell
  sharesBuy <- ifelse(tdata.num==0,0,0)
  sharesSell <- ifelse(tdata.num==0,0,0)
  capital.bal = array(1:length(tdata))
  capital.bal[1] = capital
  nShare=0 #cumulative number of asset on hand
  tolerence = 0.1 #loss tolerence in %
  stopSignal = 0 #initial value for stop signal
  tStart = 3 #initial value for tStart, last time buying future
  
  for(i in 2:length(tdata)){
     
      if(stopSignal == 1){
        pSell[i] = 1.00
        sharesSell[i] = ceiling(pSell[i]*nShare)
        amtSell[i] = sharesSell[i] * tdata[i]
        nShare = nShare - sharesSell[i]
        capital.bal[i] = capital.bal[i-1]-amtSell[i]
      }
    ###if we get strong buy signal (sig=2)
      else if(sig.num[i] == 2){
        positionSize[i] = 0.04
        tBuy[i] = positionSize[i] * capital.bal[i-1]#theoratical number to invest
        sharesBuy[i] = tBuy[i] %/% tdata[i]
        amtBuy[i] = sharesBuy[i]*tdata[i]#actual amount to buy
        position = position - positionSize[i]
        nShare = nShare + sharesBuy[i]
        capital.bal[i] = capital.bal[i-1]-amtBuy[i]
        tStart = i
      }
          
    ##if we get general buy signal, first see if the price is below Bollinger Band
      else if(sig.num[i] == 1){
          if(cdata.num[i] < BBDN.num[i]){
            positionSize[i] = 0.03
          }
          ##if the price is not below Bollinger Band then see the ATR
          else{
          #if the ATR > 3.0, set position size 2.5%
            if(ATR.num[i] < 3.0){
              positionSize[i] = 0.02}
            #otherwise set the position size 1%
            else{
              positionSize[i] = 0.01}
          }
          tBuy[i] = positionSize[i] * capital.bal[i-1]#theoratical number to invest
          sharesBuy[i] = tBuy[i] %/% tdata[i]
          amtBuy[i] = sharesBuy[i]*tdata[i]#actual amount to buy
          position = position - positionSize[i]
          nShare = nShare + sharesBuy[i]
          capital.bal[i] = capital.bal[i-1]-amtBuy[i]
          tStart = i
      }
     ###if we get strong sell signal  
      else if(sig.num[i] == -2){
          pSell[i] = 0.9
          sharesSell[i] = ceiling(pSell[i]*nShare)
          amtSell[i] = sharesSell[i] * tdata[i]
          nShare = nShare - sharesSell[i]
          capital.bal[i] = capital.bal[i-1]-amtSell[i]
      }
      ##if we get general sell signal
      else if(sig.num[i] == -1){
        ##first see if the price is higher than Bollinger Band
          if(cdata.num[i] > BBUP.num[i]){
            pSell[i] = 0.6
          }
        ##if the price is not higher than Bollinger Band then see the ATR
          else {
            if(ATR.num[i] < 3.0){
              pSell[i] = 0.50}
            else{
              pSell[i] = 0.30}
          }
          sharesSell[i] = ceiling(pSell[i]*nShare)
          amtSell[i] = sharesSell[i] * tdata[i]
          nShare = nShare - sharesSell[i]
          capital.bal[i] = capital.bal[i-1]-amtSell[i]
      }
      else if(sig.num[i] == 0){
        positionSize[i] = 0
        capital.bal[i] = capital.bal[i-1]
      }
      stopSignal = ifelse(cdata.num[i+1] < (1 - tolerence) * cdata.num[tStart],1,0) 
  }
  
}





