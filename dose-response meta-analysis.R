install.packages("metafor")
install.packages("dosresmeta")
library(metafor)
library(dosresmeta)

data("alcohol_cvd")

test<-alcohol_cvd[1:5,]
test$dose<-c(0,10,20,30,40)
test$cases<-c(500,500,500,500,500)
test$n<-c(5000,5000,5000,5000,5000)
test$logrr<-c(0,-0.2,-0.5,-0.25,0)
test$se<-c(NA,0.2,0.2,0.2,0.2)

quadrtest<-dosresmeta(formula = logrr~dose+I(dose^2),type=type,id=id,se=se,cases=cases,n=n,data = test)
with(predict(quadrtest,expo=TRUE,order=TRUE),{
  plot(dose,pred,log = "y",type = "l",xlim=c(0,45),ylim = c(0.4,2))
  lines(dose,ci.lb,lty=2)
  lines(dose,ci.ub,lty=2)
  rug(dose,quiet = TRUE)
  abline(h=1)
})
