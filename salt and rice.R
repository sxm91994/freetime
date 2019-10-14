library(ggplot2)

#salt intake data
sodium.intake=  c(170,350,700,900,1200,1400,1600,1500,1400,1400,1300)
age.low.Na=     c(0,  0.5,1,  4,  7,   11,  14,  18,  50,  65,  80)
age.high.Na=c(age.low.Na[-1],150)
salt.intake=sodium.intake*(23+35.5)/23/1000
Na.trait=data.frame(cbind(age.low.Na,age.high.Na,salt.intake,sodium.intake))

Na.deadly.trait=data.frame(cbind(age.low.Na,age.high.Na,salt.intake))
Na.deadly.trait$salt.intake=Na.deadly.trait$salt.intake*2.7

#rice intake data
crop.intake=c(0,(20+75)/2,(50+100)/2,(85+100)/2,(100+150)/2,(150+200)/2,(225+250)/2,(250+300)/2,(200+300)/2,(200+250)/2)
age.low.crop=c(0,0.5,1,2,4,7,11,14,18,65)
age.high.crop=c(age.low.crop[-1],150)
rice.intake=crop.intake
rice.trait=data.frame(cbind(age.low.crop,age.high.crop,rice.intake,crop.intake))

rice.unfair.trait=data.frame(cbind(age.low.crop,age.high.crop,rice.intake))
rice.unfair.trait$rice.intake=rice.unfair.trait$rice.intake*0.5


#function:generic aggregated food intake calculation
calc=function(obs.age,food.trait){
  total.intake=numeric(length = length(obs.age))
  age=numeric(length = length(obs.age))
  age=obs.age
  #calculate fixed aggregated result for each age strata
  food.trait$strata.intake=rep(0,nrow(food.trait))
  for (i in 1:nrow(food.trait)) {
    food.trait$strata.intake[i]=(food.trait[i,2]-food.trait[i,1])*food.trait[i,3]*365.25
  }
  #return(food.trait$strata.intake)
  
  #calculate total intake
  for (i in 1:nrow(food.trait)) {
    total.intake[age>=food.trait[i,1] & age<food.trait[i,2]]=sum(food.trait$strata.intake[0:(i-1)])+(age[age>=food.trait[i,1] & age<food.trait[i,2]]-food.trait[i,1])*food.trait[i,3]*365.25
  }

  return(total.intake)
}

calc(122,Na.deadly.trait)
calc(9,rice.trait)
calc(10,rice.trait)

curve(calc(son.age.trans,rice.trait),xname = "son.age.trans",from=0,to=6,xlab = "son's age (father's age minus 24)",ylab = "aggregated intake",lty="solid",col="#0084ff",lwd=2)
curve(calc(dad.age+24,Na.trait),xname = "dad.age",from=0,to=6,add = T,lty="solid",lwd=2,col="#8590a6")
legend("topleft", legend = c("father's salt intake", "son's rice intake"),col = c("#8590a6", "#0084ff"), lty = 1:1, cex = 0.8)


curve(calc(son.age.trans,rice.trait),xname = "son.age.trans",from=0,to=6,xlab = "son's age (father's age minus 42)",ylab = "aggregated intake",lty="solid",col="#0084ff",lwd=2)
curve(calc(dad.age+42,Na.trait),xname = "dad.age",from=0,to=6,add = T,lty="solid",lwd=2,col="#8590a6")
legend("topleft", legend = c("father's salt intake", "son's rice intake"),col = c("#8590a6", "#0084ff"), lty = 1:1, cex = 0.8)

curve(calc(son.age.trans,rice.trait),xname = "son.age.trans",from=0,to=6,xlab = "son's age (father's age minus 77)",ylab = "aggregated intake",lty="solid",col="#0084ff",lwd=2)
curve(calc(dad.age+77,Na.trait),xname = "dad.age",from=0,to=6,add = T,lty="solid",lwd=2,col="#8590a6")
legend("topleft", legend = c("father's salt intake", "son's rice intake"),col = c("#8590a6", "#0084ff"), lty = 1:1, cex = 0.8)

curve(calc(son.age.trans,rice.unfair.trait),xname = "son.age.trans",from=0,to=12,xlab = "son's age (father's age minus 42)",ylab = "aggregated intake",lty="solid",col="#0084ff",lwd=2)
curve(calc(dad.age+42,Na.deadly.trait),xname = "dad.age",from=0,to=12,add = T,lty="solid",lwd=2,col="#8590a6")
legend("topleft", legend = c("father's salt intake", "son's rice intake"),col = c("#8590a6", "#0084ff"), lty = 1:1, cex = 0.8)



curve(calc(son.age,rice.trait),xname = "son.age",from = 0,to=3,ylab = "salt intake")

wt.salt.granule=2.16*0.05^3
wt.salt.granule

curve(calc(son.age,Na.deadly.trait)/wt.salt.granule,xname = "son.age",from=0,to=15,xlab = "son's age (father's age minus 24)",ylab = "granule num.",lty="solid",col="#0084ff",lwd=2)
curve(calc(dad.age+24,rice.unfair.trait)/0.021,xname = "dad.age",from=0,to=15,add = T,lty="solid",lwd=2,col="#8590a6")
legend("topleft", legend = c("father's rice granule", "son's salt granule"),col = c("#8590a6", "#0084ff"), lty = 1:1, cex = 0.8)



curve(calc(son.age,Na.trait)/wt.salt.granule,xname = "son.age",from=0,to=2,add=F,xlab = "son's age (father's age minus 24)",ylab = "granules",lty="solid",col="#0084ff",lwd=2)
curve(calc(dad.age+24,rice.trait)/0.021,xname = "dad.age",from=0,to=2,add = T,xlab = "son's age (father's age minus 24)",ylab = "granules",lty="solid",lwd=2,col="#8590a6")
legend("topleft", legend = c("father's rice granules", "son's salt granules"),col = c("#8590a6", "#0084ff"), lty = 1:1, cex = 0.8)



#debug

obs.age=c(2,30,55)
total.intake=numeric(length = length(obs.age))
age=numeric(length = length(obs.age))
age=obs.age
#i=8
for (i in 1:nrow(Na.trait)) {
  total.intake[age>=Na.trait[i,1] & age<Na.trait[i,2]]=sum(Na.trait$strata.intake[1:(i-1)])+(age[age>=Na.trait[i,1] & age<Na.trait[i,2]]-Na.trait[i,1])*Na.trait[i,3]*365.25
}
total.intake


###########
#discarded#
###########

#determine age strata
#for (i in 1:nrow(food.trait)) {
#  flag=F
#  if (age<food.trait[i,1]) {flag=T}
#  if (flag==T) {
#    strata=i-1
#    break
#    }
#}
#return(c(strata,age.low.food[i],age.high.food[i]))

#calculate total intake
#total.intake=sum(food.trait$strata.intake[1:(strata-1)])+(age-food.trait[strata,1])*food.trait[strata,3]*365.25

##[6,+¡Ş) years old: calculated reference value based on diet pyramid
###EER for each age strata
age.low.energy=c(6,7,8,9,10,11,14,18,50,65,80)
age.high.energy=c(age.low.energy[-1],NA)
EER=c(1400,1500,1650,1750,1800,2050,2500,2250,2100,2050,1900)
EERinfo=data.frame(cbind(age.low.energy,age.high.energy,EER))
###crop intake for different energy intake level
energy.low=c(1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000)
energy.high=c(energy.low[-1],NA)
crop.intake=c(85,100,150,200,225,250,275,300,350,375,400)
rice.intake=c(85,100,150,200-(50+100)/2,22)
