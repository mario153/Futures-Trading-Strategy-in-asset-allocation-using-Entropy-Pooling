
library(quantmod)
library(TTR)
library(plyr)
library(ggplot2)
library(scales)
library(PerformanceAnalytics)
library(xts)
library(blotter)
library(FinancialInstrument)

toDate <- function(x) as.Date(x, origin = "1999-09-08")
d1 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/AD1.csv", header = TRUE, sep = ",", FUN = toDate)
d2 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/CD1.csv", header = TRUE, sep = ",", FUN = toDate)
d3 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/CF1.csv", header = TRUE, sep = ",", FUN = toDate)
d4 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/CN1.csv", header = TRUE, sep = ",", FUN = toDate)
d5 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/EC1.csv", header = TRUE, sep = ",", FUN = toDate)
d6 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/FB1.csv", header = TRUE, sep = ",", FUN = toDate)
d7 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/G_1.csv", header = TRUE, sep = ",", FUN = toDate)
d8 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/GC1.csv", header = TRUE, sep = ",", FUN = toDate) 
d9 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/GX1.csv", header = TRUE, sep = ",", FUN = toDate)
d10 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/JB1.csv", header = TRUE, sep = ",", FUN = toDate)
d11 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/ND1.csv", header = TRUE, sep = ",", FUN = toDate)
d12 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/NG1.csv", header = TRUE, sep = ",", FUN = toDate)
d13 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/HI1.csv", header = TRUE, sep = ",", FUN = toDate)
d14 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/NK1.csv", header = TRUE, sep = ",", FUN = toDate)
d15 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/OE1.csv", header = TRUE, sep = ",", FUN = toDate)
d16 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/RX1.csv", header = TRUE, sep = ",", FUN = toDate)
d17 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/SF1.csv", header = TRUE, sep = ",", FUN = toDate)
d18 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/SP1.csv", header = TRUE, sep = ",", FUN = toDate)
d19 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/TU1.csv", header = TRUE, sep = ",", FUN = toDate)
d20 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/TY1.csv", header = TRUE, sep = ",", FUN = toDate)
d21 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/VG1.csv", header = TRUE, sep = ",", FUN = toDate)
d22 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/XM1.csv", header = TRUE, sep = ",", FUN = toDate)
d23 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/YM1.csv", header = TRUE, sep = ",", FUN = toDate)
d24 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/Z_1.csv", header = TRUE, sep = ",", FUN = toDate)
d25 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/DU1.csv", header = TRUE, sep = ",", FUN = toDate)
d26 <- read.zoo("C://Columbia MAFN/4073/Single CSV Files/KE1.csv", header = TRUE, sep = ",", FUN = toDate)

AD1 <- as.xts(d1)
CD1 <- as.xts(d2)
CF1 <- as.xts(d3)
CN1 <- as.xts(d4)
EC1 <- as.xts(d5)
FB1 <- as.xts(d6)
G_1 <- as.xts(d7)
GC1 <- as.xts(d8)
GX1 <- as.xts(d9)
JB1 <- as.xts(d10)
ND1 <- as.xts(d11)
NG1 <- as.xts(d12)
HI1 <- as.xts(d13)
NK1 <- as.xts(d14)
OE1 <- as.xts(d15)
RX1 <- as.xts(d16)
SF1 <- as.xts(d17)
SP1 <- as.xts(d18)
TU1 <- as.xts(d19)
TY1 <- as.xts(d20)
VG1 <- as.xts(d21)
XM1 <- as.xts(d22)
YM1 <- as.xts(d23)
Z_1 <- as.xts(d24)
DU1 <- as.xts(d25)
KE1 <- as.xts(d26)



head(AD1)
head(Z_1)
#calculate point value

odata1<-AD1*1000#Australian Dollar
cdata1<-AD1$AD1_LAST*1000
odata2<-CD1*1000#Canadian Dollar
cdata2<-CD1$CD1_LAST*1000
odata3<-CF1*10
cdata3<-CF1$CF1_LAST*10
odata4<-CN1*100#canadian bond
cdata4<-CN1$CN1_LAST*100
odata5<-EC1*1250#Euro currency
cdata5<-EC1$EC1_LAST*1250
odata6<-FB1*1250 #swiss bond
cdata6<-FB1$FB1_LAST*1250
odata7<-G_1*1000#long gilt
cdata7<-G_1$G_1_LAST*1000
odata8<-GC1*100 #gold futures
cdata8<-GC1$GC1_LAST*100
odata9<-GX1*25
cdata9<-GX1$GX1_LAST*25
odata10<-JB1*1000 #Japan 10y bond
cdata10<-JB1$JB1_LAST*1000
odata11<-ND1*100
cdata11<-ND1$ND1_LAST*100
odata12<-NG1*10000
cdata12<-NG1$NG1_LAST*10000
odata13<-HI1*50 #in HK
cdata13<-HI1$HI1_LAST*50
odata14<-NK1*50
cdata14<-NK1$NK1_LAST*50
odata15<-OE1*1000
cdata15<-OE1$OE1_LAST*1000
odata16<-RX1*10
cdata16<-RX1$RX1_LAST*10
odata17<-SF1*1250
cdata17<-SF1$SF1_LAST*1250
odata18<-SP1*250
cdata18<-SP1$SP1_LAST*250
odata19<-TU1*15.625
cdata19<-TU1$TU1_LAST*15.625
odata20<-TY1*15.625
cdata20<-TY1$TY1_LAST*15.625
odata21<-VG1*10 #in euro
cdata21<-VG1$VG1_LAST*10
odata22<-XM1*1000
cdata22<-XM1$XM1_LAST*1000
odata23<-YM1*1000
cdata23<-YM1$YM1_LAST*1000
odata24<-Z_1*10 #in pounds
cdata24<-Z_1$Z_1_LAST*10
odata25<-DU1*1000
cdata25<-DU1$DU1_PX_LAST*1000
odata26<-KE1*1000
cdata26<-KE1$KE1_PX_LAST*1000


#The BuySellSig function generates the signal to buy/sell in Momentum Strategy
BuySellSig<-function(cdata){
    mv25=na.omit(SMA(cdata,25))
    mv90=na.omit(SMA(cdata,90))
    mv50=na.omit(SMA(cdata,50))
    mv200=na.omit(SMA(cdata,200))
    EMA12=na.omit(EMA(cdata,12))
    EMA26=na.omit(EMA(cdata,26))
    sig0=ifelse(cdata>0,0,0)
    diff1=mv25-mv90
    diff2=mv50-mv200
    sig1=ifelse(diff1>0,0,0)
    sig2=ifelse(diff2>0,0,0)
    s1=as.numeric(ifelse(diff1>0,1,-1))
    for(i in 2:length(s1)){
      dif <- s1[i] - s1[i-1]
      if(dif>0){
        sig1[i]=1
      }
      else if(dif<0){
        sig1[i]=-1
      }
      else{
        sig1[i]=0
      }
    }
    s2=as.numeric(ifelse(diff2>0,1,-1))
    for(i in 2:length(s2)){
      dif <- s2[i] - s2[i-1]
      if(dif>0){
        sig2[i]=1
      }
      else if(dif<0){
        sig2[i]=-1
      }
      else{
        sig2[i]=0
      }
    }
    MACD=EMA12-EMA26
    EMA9=na.omit(EMA(MACD),9)
    diff3=MACD-EMA9
    sig3=ifelse(diff3>=0,0,0)
    s3=as.numeric(ifelse(diff3>0,1,-1))
    for(i in 2:length(s3)){
      dif <- s3[i] - s3[i-1]
      if(dif>0){
        sig3[i]=1
      }
      else if(dif<0){
        sig3[i]=-1
      }
      else{
        sig3[i]=0
      }
    }
    sig0=sig1+sig2+sig3
    return (sig0)     
}


tsig1<-BuySellSig(cdata1)
tsig2<-BuySellSig(cdata2)
tsig3<-BuySellSig(cdata3)
tsig4<-BuySellSig(cdata4)
tsig5<-BuySellSig(cdata5)
tsig6<-BuySellSig(cdata6)
tsig7<-BuySellSig(cdata7)
tsig8<-BuySellSig(cdata8)
tsig9<-BuySellSig(cdata9)
tsig10<-BuySellSig(cdata10)
tsig11<-BuySellSig(cdata11)
tsig12<-BuySellSig(cdata12)
tsig13<-BuySellSig(cdata13)
tsig14<-BuySellSig(cdata14)
tsig15<-BuySellSig(cdata15)
tsig16<-BuySellSig(cdata16)
tsig17<-BuySellSig(cdata17)
tsig18<-BuySellSig(cdata18)
tsig19<-BuySellSig(cdata19)
tsig20<-BuySellSig(cdata20)
tsig21<-BuySellSig(cdata21)
tsig22<-BuySellSig(cdata22)
tsig23<-BuySellSig(cdata23)
tsig24<-BuySellSig(cdata24)
tsig25<-BuySellSig(cdata25)
tsig26<-BuySellSig(cdata26)

#The mod.tsig elimates those signals whose absolute values are larger than 1
mod.tsig <- function(tsig){
  for(i in 1:length(tsig)){
    if(tsig[i]==2){
      tsig[i]=1}
    else if(tsig[i]==-2){
      tsig[i]=-1}
  }
  return (tsig)
}

ret1=na.omit(ROC(type='discrete',cdata1)*mod.tsig(tsig1))
ret2=na.omit(ROC(type='discrete',cdata2)*mod.tsig(tsig2))
ret3=na.omit(ROC(type='discrete',cdata3)*mod.tsig(tsig3))
ret4=na.omit(ROC(type='discrete',cdata4)*mod.tsig(tsig4))
ret5=na.omit(ROC(type='discrete',cdata5)*mod.tsig(tsig5))
ret6=na.omit(ROC(type='discrete',cdata6)*mod.tsig(tsig6))
ret7=na.omit(ROC(type='discrete',cdata7)*mod.tsig(tsig7))
ret8=na.omit(ROC(type='discrete',cdata8)*mod.tsig(tsig8))
ret9=na.omit(ROC(type='discrete',cdata9)*mod.tsig(tsig9))
ret10=na.omit(ROC(type='discrete',cdata10)*mod.tsig(tsig10))
ret11=na.omit(ROC(type='discrete',cdata11)*mod.tsig(tsig11))
ret12=na.omit(ROC(type='discrete',cdata12)*mod.tsig(tsig12))
ret13=na.omit(ROC(type='discrete',cdata13)*mod.tsig(tsig13))
ret14=na.omit(ROC(type='discrete',cdata14)*mod.tsig(tsig14))
ret15=na.omit(ROC(type='discrete',cdata15)*mod.tsig(tsig15))
ret16=na.omit(ROC(type='discrete',cdata16)*mod.tsig(tsig16))
ret17=na.omit(ROC(type='discrete',cdata17)*mod.tsig(tsig17))
ret18=na.omit(ROC(type='discrete',cdata18)*mod.tsig(tsig18))
ret19=na.omit(ROC(type='discrete',cdata19)*mod.tsig(tsig19))
ret20=na.omit(ROC(type='discrete',cdata20)*mod.tsig(tsig20))
ret21=na.omit(ROC(type='discrete',cdata21)*mod.tsig(tsig21))
ret22=na.omit(ROC(type='discrete',cdata22)*mod.tsig(tsig22))
ret23=na.omit(ROC(type='discrete',cdata23)*mod.tsig(tsig23))
ret24=na.omit(ROC(type='discrete',cdata24)*mod.tsig(tsig24))
ret25=na.omit(ROC(type='discrete',cdata24)*mod.tsig(tsig25))
ret26=na.omit(ROC(type='discrete',cdata24)*mod.tsig(tsig26))


eq1=cumprod(1+ret1)
plot(eq1)
table.Drawdowns(ret1,top=10)
table.DownsideRisk(ret1)
charts.PerformanceSummary(ret1)

eq2=cumprod(1+ret2)
plot(eq2)
table.Drawdowns(ret2,top=10)
table.DownsideRisk(ret2)
charts.PerformanceSummary(ret2)

eq3=cumprod(1+ret3)
plot(eq3)
table.Drawdowns(ret3,top=10)
table.DownsideRisk(ret3)
charts.PerformanceSummary(ret3)

eq4=cumprod(1+ret4)
plot(eq4)
table.Drawdowns(ret4,top=10)
table.DownsideRisk(ret4)
charts.PerformanceSummary(ret4)

eq5=cumprod(1+ret5)
plot(eq5)
table.Drawdowns(ret5,top=10)
table.DownsideRisk(ret5)
charts.PerformanceSummary(ret5)

eq6=cumprod(1+ret6)
plot(eq6)
table.Drawdowns(ret6,top=10)
table.DownsideRisk(ret6)
charts.PerformanceSummary(ret6)

eq7=cumprod(1+ret7)
plot(eq7)
table.Drawdowns(ret7,top=10)
table.DownsideRisk(ret7)
charts.PerformanceSummary(ret7)


eq8=cumprod(1+ret8)
plot(eq8)
table.Drawdowns(ret8,top=10)
table.DownsideRisk(ret8)
charts.PerformanceSummary(ret8)

eq9=cumprod(1+ret9)
plot(eq9)
table.Drawdowns(ret9,top=10)
table.DownsideRisk(ret9)
charts.PerformanceSummary(ret9)

eq10=cumprod(1+ret10)
plot(eq10)
table.Drawdowns(ret10,top=10)
table.DownsideRisk(ret10)
charts.PerformanceSummary(ret10)

eq11=cumprod(1+ret11)
plot(eq11)
table.Drawdowns(ret11,top=10)
table.DownsideRisk(ret11)
charts.PerformanceSummary(ret11)

eq12=cumprod(1+ret12)
plot(eq12)
table.Drawdowns(ret12,top=10)
table.DownsideRisk(ret12)
charts.PerformanceSummary(ret12)

eq13=cumprod(1+ret13)
plot(eq13)
table.Drawdowns(ret13,top=10)
table.DownsideRisk(ret13)
charts.PerformanceSummary(ret13)

eq14=cumprod(1+ret14)
plot(eq14)
table.Drawdowns(ret14,top=10)
table.DownsideRisk(ret14)
charts.PerformanceSummary(ret14)

eq15=cumprod(1+ret15)
plot(eq15)
table.Drawdowns(ret15,top=10)
table.DownsideRisk(ret15)
charts.PerformanceSummary(ret15)

eq16=cumprod(1+ret16)
plot(eq16)
table.Drawdowns(ret16,top=10)
table.DownsideRisk(ret16)
charts.PerformanceSummary(ret16)

eq17=cumprod(1+ret17)
plot(eq17)
table.Drawdowns(ret17,top=10)
table.DownsideRisk(ret17)
charts.PerformanceSummary(ret17)

eq18=cumprod(1+ret18)
plot(eq18)
table.Drawdowns(ret18,top=10)
table.DownsideRisk(ret18)
charts.PerformanceSummary(ret18)

eq19=cumprod(1+ret19)
plot(eq19)
table.Drawdowns(ret19,top=10)
table.DownsideRisk(ret19)
charts.PerformanceSummary(ret19)

eq20=cumprod(1+ret20)
plot(eq20)
table.Drawdowns(ret20,top=10)
table.DownsideRisk(ret20)
charts.PerformanceSummary(ret20)

eq21=cumprod(1+ret21)
plot(eq21)
table.Drawdowns(ret21,top=10)
table.DownsideRisk(ret21)
charts.PerformanceSummary(ret21)

eq22=cumprod(1+ret22)
plot(eq22)
table.Drawdowns(ret22,top=10)
table.DownsideRisk(ret22)
charts.PerformanceSummary(ret22)

eq23=cumprod(1+ret23)
plot(eq23)
table.Drawdowns(ret23,top=10)
table.DownsideRisk(ret23)
charts.PerformanceSummary(ret23)

eq24=cumprod(1+ret24)
plot(eq24)
table.Drawdowns(ret24,top=10)
table.DownsideRisk(ret24)
charts.PerformanceSummary(ret24)

eq25=cumprod(1+ret25)
plot(eq25)
table.Drawdowns(ret25,top=10)
table.DownsideRisk(ret25)
charts.PerformanceSummary(ret25)

eq26=cumprod(1+ret26)
plot(eq24)
table.Drawdowns(ret26,top=10)
table.DownsideRisk(ret26)
charts.PerformanceSummary(ret26)

###trading


index1 <- ifelse(tsig1>3,0,1)
index2 <- ifelse(tsig2>3,0,1)
index3 <- ifelse(tsig3>3,0,1)
index4 <- ifelse(tsig4>3,0,1)
index5 <- ifelse(tsig5>3,0,1)
index6 <- ifelse(tsig6>3,0,1)
index7 <- ifelse(tsig7>3,0,1)
index8 <- ifelse(tsig8>3,0,1)
index9 <- ifelse(tsig9>3,0,1)
index10 <- ifelse(tsig10>3,0,1)
index11 <- ifelse(tsig11>3,0,1)
index12 <- ifelse(tsig12>3,0,1)
index13 <- ifelse(tsig13>3,0,1)
index14 <- ifelse(tsig14>3,0,1)
index15 <- ifelse(tsig15>3,0,1)
index16 <- ifelse(tsig16>3,0,1)
index17 <- ifelse(tsig17>3,0,1)
index18 <- ifelse(tsig18>3,0,1)
index19 <- ifelse(tsig19>3,0,1)
index20 <- ifelse(tsig20>3,0,1)
index21<- ifelse(tsig21>3,0,1)
index22 <- ifelse(tsig22>3,0,1)
index23<- ifelse(tsig23>3,0,1)
index24 <- ifelse(tsig24>3,0,1)
index25 <- ifelse(tsig25>3,0,1)
index26 <- ifelse(tsig26>3,0,1)


tdata1<-mod.tsig(tsig1) * cdata1
tdata2<-mod.tsig(tsig2) * cdata2
tdata3<-mod.tsig(tsig3) * cdata3
tdata4<-mod.tsig(tsig4) * cdata4
tdata5<-mod.tsig(tsig5) * cdata5
tdata6<-mod.tsig(tsig6) * cdata6
tdata7<-mod.tsig(tsig7) * cdata7
tdata8<-mod.tsig(tsig8) * cdata8
tdata9<-mod.tsig(tsig9) * cdata9
tdata10<-mod.tsig(tsig10) * cdata10
tdata11<-mod.tsig(tsig11) * cdata11
tdata12<-mod.tsig(tsig12) * cdata12
tdata13<-mod.tsig(tsig13) * cdata13
tdata14<-mod.tsig(tsig14) * cdata14
tdata15<-mod.tsig(tsig15) * cdata15
tdata16<-mod.tsig(tsig16) * cdata16
tdata17<-mod.tsig(tsig17) * cdata17
tdata18<-mod.tsig(tsig18) * cdata18
tdata19<-mod.tsig(tsig19) * cdata19
tdata20<-mod.tsig(tsig20) * cdata20
tdata21<-mod.tsig(tsig21) * cdata21
tdata22<-mod.tsig(tsig22) * cdata22
tdata23<-mod.tsig(tsig23) * cdata23
tdata24<-mod.tsig(tsig24) * cdata24
tdata25<-mod.tsig(tsig25) * cdata25
tdata26<-mod.tsig(tsig26) * cdata26

mod.cdata1 <- index1*cdata1
mod.cdata2 <- index2*cdata2
mod.cdata3 <- index3*cdata3
mod.cdata4 <- index4*cdata4
mod.cdata5 <- index5*cdata5
mod.cdata6 <- index6*cdata6
mod.cdata7 <- index7*cdata7
mod.cdata8 <- index8*cdata8
mod.cdata9 <- index9*cdata9
mod.cdata10 <- index10*cdata10
mod.cdata11 <- index11*cdata11
mod.cdata12 <- index12*cdata12
mod.cdata13 <- index13*cdata13
mod.cdata14 <- index14*cdata14
mod.cdata15 <- index15*cdata15
mod.cdata16 <- index16*cdata16
mod.cdata17 <- index17*cdata17
mod.cdata18 <- index18*cdata18
mod.cdata19 <- index19*cdata19
mod.cdata20 <- index20*cdata20
mod.cdata21 <- index21*cdata21
mod.cdata22 <- index22*cdata22
mod.cdata23 <- index23*cdata23
mod.cdata24 <- index24*cdata24
mod.cdata25 <- index24*cdata25
mod.cdata26 <- index24*cdata26





####BELOW IS THE MAIN FUNCTION
#cat is catagory of the futures contract, it will include 4 values as below
#"currency"
#"commodity"
#"index"
#"bond"

trade <- function(cdata,mod.cdata,odata,tdata,tsig,index,capital,cat,tolerance){
  
  mod.cdata <- index*cdata
  RSI = na.omit(RSI(cdata))*index
  BBup <- na.omit(BBands(cdata)$up)*index
  BBdn <- na.omit(BBands(cdata)$dn)*index
  amtBuy <- ifelse(tdata==0,0,0)
  amtSell <- ifelse(tdata==0,0,0)
  tCost <- ifelse(tdata==0,0,0)#transaction cost
  sharesBuy <- ifelse(tdata==0,0,0)
  sharesSell <- ifelse(tdata==0,0,0)
  capital.bal <- ifelse(tdata==0,0,0)
  capital.bal[1] = capital
  portf <- ifelse(tdata==0,0,0)
  portf[1] = capital
  pnl <- ifelse(tdata==0,0,0)
  nShare=0 #cumulative number of asset on hand
  stopSignal <- ifelse(tdata==0,0,0)
  tStart = 1 #initial value for tStart, last time buying future
  #the function below determines the first buying action 
  while(tsig[tStart]!=1){
    tStart=tStart+1
  }
  if(cat=="index"){
    for(i in 2:length(tdata)){
      if(stopSignal[i] == 1){
        sharesSell[i]=nShare
        tCost[i]=1.15*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * mod.cdata[i] - tCost[i]
        nShare = 0
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ###if we get strong buy signal (sig=2), see the Bollinger Bands first
      else if(tsig[i]>=2){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 30
        }
        # even if price doesn't fall below Bollinger Bands
        # Still need to see ATR and RSI to determine how much to buy
        else {
          if(RSI[i] < 30){
            sharesBuy[i] = 30}
          else{
            sharesBuy[i] = 10}
        }
        tCost[i]=1.15*sharesBuy[i]
        amtBuy[i] = sharesBuy[i]*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      
      ##if we get general buy signal, first see if the price is below Bollinger Band
      else if(tsig[i] == 1){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 20
        }
        ##if the price is not below Bollinger Band then see the ATR and RSI
        else{
          if(RSI[i] < 30){
            sharesBuy[i] = 20}
          else{
            sharesBuy[i] = 8}
        }
        tCost[i]=1.15*sharesBuy[i]
        amtBuy[i] = as.numeric(sharesBuy[i])*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      ###if we get strong sell signal  
      else if(tsig[i]<=-2){
        if(RSI[i] > 70){
          sharesSell[i] = nShare}
        else{
          sharesSell[i] = ceiling(0.8*nShare)}
        tCost[i]=1.15*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ##if we get general sell signal
      else if(tsig[i] == -1){
        ##first see if the price is higher than Bollinger Band
        if(mod.cdata[i] > BBup[i]){
          sharesSell[i] = nShare
        }
        ##if the price is not higher than Bollinger Band then see the ATR and RSI
        else {
          if(RSI[i] > 70){
            sharesSell[i] = nShare}
          else{
            sharesSell[i] = ceiling(0.5*nShare)}
        }
        tCost[i]=1.15*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      else if(tsig[i] == 0){
        sharesBuy[i] = 0
        capital.bal[i] = capital.bal[i-1]
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      if(i > tStart & i < length(tdata)){
        diff<-as.numeric(mod.cdata[i+1])-((1-tolerance)*as.numeric(mod.cdata[tStart]))
        stopSignal[i+1] = ifelse(diff<0,1,0) 
      }
    }
  }
  
  else if(cat=="currency"){
    for(i in 2:length(tdata)){
      if(stopSignal[i] == 1){
        sharesSell[i]=nShare
        tCost[i]=1.6*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * mod.cdata[i] - tCost[i]
        nShare = 0
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ###if we get strong buy signal (sig=2), see the Bollinger Bands first
      else if(tsig[i]>=2){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 30
        }
        # even if price doesn't fall below Bollinger Bands
        # Still need to see ATR and RSI to determine how much to buy
        else {
          if(RSI[i] < 30){
            sharesBuy[i] = 30}
          else{
            sharesBuy[i] = 10}
        }
        tCost[i]=1.6*sharesBuy[i]
        amtBuy[i] = sharesBuy[i]*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      
      ##if we get general buy signal, first see if the price is below Bollinger Band
      else if(tsig[i] == 1){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 20
        }
        ##if the price is not below Bollinger Band then see the ATR and RSI
        else{
          if(RSI[i] < 30){
            sharesBuy[i] = 20}
          else{
            sharesBuy[i] = 8}
        }
        tCost[i]=1.6*sharesBuy[i]
        amtBuy[i] = as.numeric(sharesBuy[i])*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      ###if we get strong sell signal  
      else if(tsig[i]<=-2){
        if(RSI[i] > 70){
          sharesSell[i] = nShare}
        else{
          sharesSell[i] = ceiling(0.8*nShare)}
        tCost[i]=1.6*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ##if we get general sell signal
      else if(tsig[i] == -1){
        ##first see if the price is higher than Bollinger Band
        if(mod.cdata[i] > BBup[i]){
          sharesSell[i] = nShare
        }
        ##if the price is not higher than Bollinger Band then see the ATR and RSI
        else {
          if(RSI[i] > 70){
            sharesSell[i] = nShare}
          else{
            sharesSell[i] = ceiling(0.5*nShare)}
        }
        tCost[i]=1.6*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      else if(tsig[i] == 0){
        sharesBuy[i] = 0
        capital.bal[i] = capital.bal[i-1]
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      if(i > tStart & i < length(tdata)){
        diff<-as.numeric(mod.cdata[i+1])-((1-tolerance)*as.numeric(mod.cdata[tStart]))
        stopSignal[i+1] = ifelse(diff<0,1,0) 
      }
    }
  }
  
  else if(cat=="commodity"){
    for(i in 2:length(tdata)){
      if(stopSignal[i] == 1){
        sharesSell[i]=nShare
        tCost[i]=2.03*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * mod.cdata[i] - tCost[i]
        nShare = 0
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ###if we get strong buy signal (sig=2), see the Bollinger Bands first
      else if(tsig[i]>=2){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 30
        }
        # even if price doesn't fall below Bollinger Bands
        # Still need to see ATR and RSI to determine how much to buy
        else {
          if(RSI[i] < 30){
            sharesBuy[i] = 30}
          else{
            sharesBuy[i] = 10}
        }
        tCost[i]=2.03*sharesBuy[i]
        amtBuy[i] = sharesBuy[i]*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      
      ##if we get general buy signal, first see if the price is below Bollinger Band
      else if(tsig[i] == 1){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 20
        }
        ##if the price is not below Bollinger Band then see the ATR and RSI
        else{
          if(RSI[i] < 30){
            sharesBuy[i] = 20}
          else{
            sharesBuy[i] = 8}
        }
        tCost[i]=2.03*sharesBuy[i]
        amtBuy[i] = as.numeric(sharesBuy[i])*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      ###if we get strong sell signal  
      else if(tsig[i]<=-2){
        if(RSI[i] > 70){
          sharesSell[i] = nShare}
        else{
          sharesSell[i] = ceiling(0.8*nShare)}
        tCost[i]=2.03*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ##if we get general sell signal
      else if(tsig[i] == -1){
        ##first see if the price is higher than Bollinger Band
        if(mod.cdata[i] > BBup[i]){
          sharesSell[i] = nShare
        }
        ##if the price is not higher than Bollinger Band then see the ATR and RSI
        else {
          if(RSI[i] > 70){
            sharesSell[i] = nShare}
          else{
            sharesSell[i] = ceiling(0.5*nShare)}
        }
        tCost[i]=2.03*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      else if(tsig[i] == 0){
        sharesBuy[i] = 0
        capital.bal[i] = capital.bal[i-1]
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      if(i > tStart & i < length(tdata)){
        diff<-as.numeric(mod.cdata[i+1])-((1-tolerance)*as.numeric(mod.cdata[tStart]))
        stopSignal[i+1] = ifelse(diff<0,1,0) 
      }
    }
  }
  
  else if(cat=="bond"){
    for(i in 2:length(tdata)){
      if(stopSignal[i] == 1){
        sharesSell[i]=nShare
        tCost[i]=0.6*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * mod.cdata[i] - tCost[i]
        nShare = 0
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ###if we get strong buy signal (sig=2), see the Bollinger Bands first
      else if(tsig[i]>=2){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 30
        }
        # even if price doesn't fall below Bollinger Bands
        # Still need to see ATR and RSI to determine how much to buy
        else {
          if(RSI[i] < 30){
            sharesBuy[i] = 30}
          else{
            sharesBuy[i] = 10}
        }
        tCost[i]=0.6*sharesBuy[i]
        amtBuy[i] = sharesBuy[i]*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      
      ##if we get general buy signal, first see if the price is below Bollinger Band
      else if(tsig[i] == 1){
        if(mod.cdata[i] < BBdn[i]){
          sharesBuy[i] = 20
        }
        ##if the price is not below Bollinger Band then see the ATR and RSI
        else{
          if(RSI[i] < 30){
            sharesBuy[i] = 20}
          else{
            sharesBuy[i] = 8}
        }
        tCost[i]=0.6*sharesBuy[i]
        amtBuy[i] = as.numeric(sharesBuy[i])*tdata[i] + tCost[i]#actual amount to buy
        nShare = nShare + as.numeric(sharesBuy[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtBuy[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
        tStart = i
      }
      ###if we get strong sell signal  
      else if(tsig[i]<=-2){
        if(RSI[i] > 70){
          sharesSell[i] = nShare}
        else{
          sharesSell[i] = ceiling(0.8*nShare)}
        tCost[i]=0.6*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      ##if we get general sell signal
      else if(tsig[i] == -1){
        ##first see if the price is higher than Bollinger Band
        if(mod.cdata[i] > BBup[i]){
          sharesSell[i] = nShare
        }
        ##if the price is not higher than Bollinger Band then see the ATR and RSI
        else {
          if(RSI[i] > 70){
            sharesSell[i] = nShare}
          else{
            sharesSell[i] = ceiling(0.5*nShare)}
        }
        tCost[i]=0.6*sharesSell[i]
        amtSell[i] = as.numeric(sharesSell[i]) * tdata[i] + tCost[i]
        nShare = nShare - as.numeric(sharesSell[i])
        capital.bal[i] = capital.bal[i-1]-as.numeric(amtSell[i])
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      else if(tsig[i] == 0){
        sharesBuy[i] = 0
        capital.bal[i] = capital.bal[i-1]
        portf[i] = capital.bal[i] + (nShare*mod.cdata[i])
        pnl[i] = (as.numeric(portf[i])-as.numeric(portf[i-1]))/portf[i-1]
      }
      if(i > tStart & i < length(tdata)){
        diff<-as.numeric(mod.cdata[i+1])-((1-tolerance)*as.numeric(mod.cdata[tStart]))
        stopSignal[i+1] = ifelse(diff<0,1,0) 
      }
    }
  }
  
  
  #plot(portf,main="Portfolio Value")
  
  #ret <- portf/capital
  
  #plot(ret,main="Cumulative Return")
  
  #plot(pnl,main="PnL")
  
  return (pnl)
}

setwd("C://Columbia MAFN/4073/Momentum PnL CSV")

#when trade function returns the plot of portfolio value
trade(cdata1,mod.cdata1,odata1,tdata1,tsig1,index1,capital=100000,cat="currency",tolerance=0.5)
trade(cdata2,mod.cdata2,odata2,tdata2,tsig2,index2,capital=100000,cat="currency",tolerance=0.2)
trade(cdata3,mod.cdata3,odata3,tdata3,tsig3,index3,capital=100000,cat="index",tolerance=0.5)
trade(cdata4,mod.cdata4,odata4,tdata4,tsig4,index4,capital=100000,cat="bond",tolerance=0.2)
trade(cdata5,mod.cdata5,odata5,tdata5,tsig5,index5,capital=100000,cat="currency",tolerance=0.3)
trade(cdata6,mod.cdata6,odata6,tdata6,tsig6,index6,capital=100000,cat="bond",tolerance=0.2)
trade(cdata7,mod.cdata7,odata7,tdata7,tsig7,index7,capital=100000,cat="bond",tolerance=0.2)
trade(cdata8,mod.cdata8,odata8,tdata8,tsig8,index8,capital=100000,cat="commodity",tolerance=0.5)
trade(cdata9,mod.cdata9,odata9,tdata9,tsig9,index9,capital=100000,cat="index",tolerance=0.5)
trade(cdata10,mod.cdata10,odata10,tdata10,tsig10,index10,capital=100000,cat="bond",tolerance=0.2)
trade(cdata11,mod.cdata11,odata11,tdata11,tsig11,index11,capital=100000,cat="index",tolerance=0.5)
trade(cdata12,mod.cdata12,odata12,tdata12,tsig12,index12,capital=100000,cat="commodity",tolerance=0.3)
trade(cdata13,mod.cdata13,odata13,tdata13,tsig13,index13,capital=100000,cat="index",tolerance=0.5)
trade(cdata14,mod.cdata14,odata14,tdata14,tsig14,index14,capital=100000,cat="index",tolerance=0.5)
trade(cdata15,mod.cdata15,odata15,tdata15,tsig15,index15,capital=100000,cat="bond",tolerance=0.2)
trade(cdata16,mod.cdata16,odata16,tdata16,tsig16,index16,capital=100000,cat="bond",tolerance=0.2)
trade(cdata17,mod.cdata17,odata17,tdata17,tsig17,index17,capital=100000,cat="commodity",tolerance=0.2)
trade(cdata18,mod.cdata18,odata18,tdata18,tsig18,index18,capital=100000,cat="index",tolerance=0.5)
trade(cdata19,mod.cdata19,odata19,tdata19,tsig19,index19,capital=100000,cat="bond",tolerance=0.2)
trade(cdata20,mod.cdata20,odata20,tdata20,tsig20,index20,capital=100000,cat="bond",tolerance=0.2)
trade(cdata21,mod.cdata21,odata21,tdata21,tsig21,index21,capital=100000,cat="index",tolerance=0.4)
trade(cdata22,mod.cdata22,odata22,tdata22,tsig22,index22,capital=100000,cat="bond",tolerance=0.2)
trade(cdata23,mod.cdata23,odata23,tdata23,tsig23,index23,capital=100000,cat="bond",tolerance=0.2)
trade(cdata24,mod.cdata24,odata24,tdata24,tsig24,index24,capital=100000,cat="index",tolerance=0.5)
trade(cdata25,mod.cdata25,odata25,tdata25,tsig25,index25,capital=100000,cat="bond",tolerance=0.2)
trade(cdata26,mod.cdata26,odata26,tdata26,tsig26,index26,capital=100000,cat="bond",tolerance=0.2)


#when the trade function returns the pnl of each future contract
pnl1=trade(cdata1,mod.cdata1,odata1,tdata1,tsig1,index1,capital=100000,cat="currency",tolerance=0.5)
pnl2=trade(cdata2,mod.cdata2,odata2,tdata2,tsig2,index2,capital=100000,cat="currency",tolerance=0.2)
pnl3=trade(cdata3,mod.cdata3,odata3,tdata3,tsig3,index3,capital=100000,cat="index",tolerance=0.5)
pnl4=trade(cdata4,mod.cdata4,odata4,tdata4,tsig4,index4,capital=100000,cat="bond",tolerance=0.2)
pnl5=trade(cdata5,mod.cdata5,odata5,tdata5,tsig5,index5,capital=100000,cat="currency",tolerance=0.3)
pnl6=trade(cdata6,mod.cdata6,odata6,tdata6,tsig6,index6,capital=100000,cat="bond",tolerance=0.2)
pnl7=trade(cdata7,mod.cdata7,odata7,tdata7,tsig7,index7,capital=100000,cat="bond",tolerance=0.2)
pnl8=trade(cdata8,mod.cdata8,odata8,tdata8,tsig8,index8,capital=100000,cat="commodity",tolerance=0.5)
pnl9=trade(cdata9,mod.cdata9,odata9,tdata9,tsig9,index9,capital=100000,cat="index",tolerance=0.5)
pnl10=trade(cdata10,mod.cdata10,odata10,tdata10,tsig10,index10,capital=100000,cat="bond",tolerance=0.2)
pnl11=trade(cdata11,mod.cdata11,odata11,tdata11,tsig11,index11,capital=100000,cat="index",tolerance=0.5)
pnl12=trade(cdata12,mod.cdata12,odata12,tdata12,tsig12,index12,capital=100000,cat="commodity",tolerance=0.3)
pnl13=trade(cdata13,mod.cdata13,odata13,tdata13,tsig13,index13,capital=100000,cat="index",tolerance=0.5)
pnl14=trade(cdata14,mod.cdata14,odata14,tdata14,tsig14,index14,capital=100000,cat="index",tolerance=0.5)
pnl15=trade(cdata15,mod.cdata15,odata15,tdata15,tsig15,index15,capital=100000,cat="bond",tolerance=0.2)
pnl16=trade(cdata16,mod.cdata16,odata16,tdata16,tsig16,index16,capital=100000,cat="bond",tolerance=0.2)
pnl17=trade(cdata17,mod.cdata17,odata17,tdata17,tsig17,index17,capital=100000,cat="commodity",tolerance=0.2)
pnl18=trade(cdata18,mod.cdata18,odata18,tdata18,tsig18,index18,capital=100000,cat="index",tolerance=0.5)
pnl19=trade(cdata19,mod.cdata19,odata19,tdata19,tsig19,index19,capital=100000,cat="bond",tolerance=0.2)
pnl20=trade(cdata20,mod.cdata20,odata20,tdata20,tsig20,index20,capital=100000,cat="bond",tolerance=0.2)
pnl21=trade(cdata21,mod.cdata21,odata21,tdata21,tsig21,index21,capital=100000,cat="index",tolerance=0.4)
pnl22=trade(cdata22,mod.cdata22,odata22,tdata22,tsig22,index22,capital=100000,cat="bond",tolerance=0.2)
pnl23=trade(cdata23,mod.cdata23,odata23,tdata23,tsig23,index23,capital=100000,cat="bond",tolerance=0.2)
pnl24=trade(cdata24,mod.cdata24,odata24,tdata24,tsig24,index24,capital=100000,cat="index",tolerance=0.5)
pnl25=trade(cdata25,mod.cdata25,odata25,tdata25,tsig25,index25,capital=100000,cat="bond",tolerance=0.2)
pnl26=trade(cdata26,mod.cdata26,odata26,tdata26,tsig26,index26,capital=100000,cat="bond",tolerance=0.2)


write.zoo(pnl1,"pnl_AD1.csv",sep=',')
write.zoo(pnl2,"pnl_CD1.csv",sep=',')
write.zoo(pnl3,"pnl_CF1.csv",sep=',')
write.zoo(pnl4,"pnl_CN1.csv",sep=',')
write.zoo(pnl5,"pnl_EC1.csv",sep=',')
write.zoo(pnl6,"pnl_FB1.csv",sep=',')
write.zoo(pnl7,"pnl_G_1.csv",sep=',')
write.zoo(pnl8,"pnl_GC1.csv",sep=',')
write.zoo(pnl9,"pnl_GX1.csv",sep=',')
write.zoo(pnl10,"pnl_JB1.csv",sep=',')
write.zoo(pnl11,"pnl_ND1.csv",sep=',')
write.zoo(pnl12,"pnl_NG1.csv",sep=',')
write.zoo(pnl13,"pnl_HI1.csv",sep=',')
write.zoo(pnl14,"pnl_NK1.csv",sep=',')
write.zoo(pnl15,"pnl_OE1.csv",sep=',')
write.zoo(pnl16,"pnl_RX1.csv",sep=',')
write.zoo(pnl17,"pnl_SF1.csv",sep=',')
write.zoo(pnl18,"pnl_SP1.csv",sep=',')
write.zoo(pnl19,"pnl_TU1.csv",sep=',')
write.zoo(pnl20,"pnl_TY1.csv",sep=',')
write.zoo(pnl21,"pnl_VG1.csv",sep=',')
write.zoo(pnl22,"pnl_XM1.csv",sep=',')
write.zoo(pnl23,"pnl_YM1.csv",sep=',')
write.zoo(pnl24,"pnl_Z_1.csv",sep=',')
write.zoo(pnl25,"pnl_DU1.csv",sep=',')
write.zoo(pnl26,"pnl_KE1.csv",sep=',')

PNL1<-cbind(pnl1,pnl2,pnl3,pnl4,pnl5,pnl6,pnl7,pnl8,pnl9,pnl10,pnl11,pnl12,pnl13,pnl14,pnl15,pnl16,pnl17,pnl18,pnl19,pnl20,pnl21,pnl22,pnl23,pnl24,pnl25,pnl26)
for (i in 1:nrow(PNL1)){
  for (j in 1:ncol(PNL1)){
    if((toString(PNL1[i,j]) == "NA")){
      PNL1[i,j]=0
    }        
  }
} 

write.zoo(PNL1,"PNL_Momentum.csv",sep=',')





