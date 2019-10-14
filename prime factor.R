install.packages("numbers")
library(numbers)

for (i in 1:100) {
  if (i%%2==1) {next} 
  if (length(primeFactors(6541367000+i))==2) {print(primeFactors(6541367000+i))}
}