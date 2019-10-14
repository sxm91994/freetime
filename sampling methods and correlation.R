test1<-rep(1,each=100)
test2<-rep(1,each=100) #generate a vector with length=100 and each value equals 1

truesmp<-rnorm(100,mean=120,sd=30)
method1<-truesmp+rnorm(100,mean=0,sd=30)
method2<-truesmp+rnorm(100,mean=0,sd=40)
result<-cat("sample  : ","mean = ",mean(truesmp)," sd = ",sd(truesmp),"\n","method 1: ","mean = ",mean(method1)," sd = ",sd(method1),"\n","method 2: ","mean = ",mean(method2)," sd = ",sd(method2),sep = "")

plot(method1,method2)
plot(truesmp,method1)
plot(truesmp,method2)
cor(method1,method2)
cor(truesmp,method1)
cor(truesmp,method2)
