#everyday spend x fishes for upgrading, which x leads the quickliest full collection?

#------- Function 1: house level up cost prediction -------#

#costprd.o21(lv,rarity)
#csumprd.o21(baselv,rarity,lvupneed)

lv<-c(21:35)
rarity<-lv-lv
lvin<-c(21,21,21,22,22,23,24,26,27,31,32,36,29,30,31,26)
costin<-c(7000,8000,6000,6200,7200,7400,6600,10500,10800,16000,16400,22500,12900,13200,18000,12000)
rarityin<-c(1,2,0,0,1,1,0,0,0,0,0,0,1,1,1,1)
lv5numin<-trunc((lvin-1)/5)
resid5in<-(lvin-1)%%5
#plot(lv,cost)

model<-glm(costin~resid5in*lv5numin+rarityin*lv5numin+I(lv5numin*(lv5numin-1)))
summary(model)


#single upgrade cost
costprd.o21<-function(lv,rarity){
  costpred=((lv-1)%%5)*((trunc((lv-1)/5)-2)*100)+1000*rarity+500*rarity*(trunc((lv-1)/5)-4)+6000+3500*(trunc((lv-1)/5)-4)+(trunc((lv-1)/5)-4)*(trunc((lv-1)/5)-3)/2*1000
  return(costpred)
}

for (i in c(0:9)) {
  print(costprd.o21(i+23,1))
}

costprd.o21(37,1)

#cumulative cost for several consecutive upgrade
csumprd.o21<-function(baselv,rarity,lvupneed){
  lvneed<-c(1:lvupneed)-1
  sumcost=0
  for (i in lvneed) {
    sumcost=sumcost+costprd.o21(i+baselv,rarity)
  }
  return(sumcost)
}

csumprd.o21(41,0,1)



#------- Function 2: house earning prediction -------#

#lv21:600,700,800
#lv up v.earn inc:+20,+20,+20
#lv up cap. inc: 160,240,320
earnspd<-function(lv,rarity){
  baseearn21<-c(600,700,800)
  earnabi<-baseearn21[rarity+1]+(lv-21)*20
  return(earnabi)
}
cumearn<-function(lv,rarity,time){
  basecap<-c(4800,8400,12800)
  inc<-c(160,240,320)
  cap<-basecap[rarity+1]+(lv-21)*inc[rarity+1]
  sumearn<-earnspd(lv,rarity)*time
  ifelse(sumearn<=cap,return(sumearn),return(cap))
}

sumearn=9000
cap=4800
ifelse(sumearn<=cap,print(sumearn),print(cap))

earnspd(41,0)
cumearn(41,0,12)

#------- Table of my holdings -------#
currlevel<-c(21,21,21,21,21,21,21,21,
             21,22,22,24,26,27,41,21,
             21,21,21,21,21,21,21,21,
             22,26,37,21,21,21,21,21,
             21,21,21,21,21,21,22,31)
rare<-c(0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,1,
        1,1,1,1,1,1,1,1,
        1,1,1,2,2,2,2,2,
        2,2,2,2,2,2,2,2)
holdings<-data.frame(currlevel,rare)
holdings$earnings<-NA
holdings$earnabil<-NA

holdings$earnabil<-earnspd(currlevel,rare)



#Writing like this causes ifelse function disabled
#holdings$earnings<-cumearn(holdings$currlevel,holdings$rare,runtime)

sum(holdings$earnings)

#------- Function 3: how to spend the fishes -------#

#------- Total earning per 12hr -------#



upg.yn<-function(draws,check.t.intvl,currholdings) {
runtime=check.t.intvl
#without lv up
for (i in c(1:40)) {
  currholdings$earnings[i]<-cumearn(currholdings$currlevel[i],currholdings$rare[i],runtime)
}
time.no.up<-6000*(20-2)*draws/sum(currholdings$earnings)
#with cheapest lv up
min.lvup.cost<-min(costprd.o21(currholdings$currlevel,currholdings$rare))
time.with.up<-(6000*(20-2)*draws+min.lvup.cost)/(sum(currholdings$earnings)+20*check.t.intvl)
ifelse(time.with.up<time.no.up,return(1),return(0))
}

upg.yn(70,8,holdings)

#predict the fastest upgrading matrix
holdings.prd<-holdings
runtime=8
while (upg.yn(70,8,holdings.prd)==1) {
  holdings.prd$lvupcost<-NA
  holdings.prd$lvupcost<-costprd.o21(holdings.prd$currlevel,holdings.prd$rare)
  o<-order(holdings.prd$lvupcost)
  holdings.prd<-holdings.prd[o,]
  holdings.prd$currlevel[1]<-holdings.prd$currlevel[1]+1
  holdings.prd$earnabil[1]<-earnspd(holdings.prd$currlevel[1],holdings.prd$rare[1])
  holdings.prd$earnings[1]<-cumearn(holdings.prd$currlevel[1],holdings.prd$rare[1],runtime)
}
holdings.prd


for (i in c(1:40)) {
  holdings.prd$earnings[i]<-cumearn(holdings.prd$currlevel[i],holdings.prd$rare[i],runtime)
}
achieve.time<-6000*(20-2)*70/sum(holdings.prd$earnings)*runtime/24

achieve.time




######using poisson distribution to generate expected probability
times<-c(1:100)
parlmd<-times*0.0233
predprob<-times-times
for (i in c(1:100)) {
  predprob[i]<-ppois(0,parlmd[i])
}
plot(times,predprob)
?ppois()
