install.packages("ggplot2")
library(ggplot2)

crop<-c("wheat","corn","carrot","sugarcane","cotton","strawberry","tomato","pine tree","potato","cacao","rubber tree","silk","pepper","rice","rose","jasmine","coffee plant")
seedprc<-c(0,1,2,3,4,5,6,7,8,9,15,20,11,7,18,25,33)
harvprc<-c(1,3,5,7,9,11,13,15,17,19,29,33,20,14,28,37,48)
maturetime<-c(2,5,10,20,30,60,120,180,240,480,720,900,300,80,150,210,360)
fieldnum<-71

earn_per_field=harvprc-seedprc
earn<-(harvprc-seedprc)*fieldnum
earnspd<-earn/maturetime

pricelst<-data.frame(crop,seedprc,harvprc,maturetime,earn_per_field,earn,earnspd)

pricelst[order(pricelst$maturetime),]

attach(pricelst)

plot(maturetime,earnspd)

ggplot(pricelst,aes(x=maturetime,y=earnspd))+
  geom_point()+
  geom_text(aes(x=maturetime+10,y=earnspd,label=crop),hjust=0)

shortage<-6000
moves<-ceiling(shortage/earn)
totaltime<-moves*maturetime
pricelst$moves<-moves
pricelst$totaltime<-totaltime
pricelst$totaltimett<-paste(floor(totaltime/60),"h",totaltime%%60,"min",sep = "")

ggplot(pricelst,aes(x=moves,y=totaltime))+
  geom_point()+
  geom_text(aes(x=moves+1,y=totaltime,label=crop),hjust=0)+
  xlim(5,100)


#bread
breadtime<-2+5*6*(48/12)
breadearn<-5*6*48/12
