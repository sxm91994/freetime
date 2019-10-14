install.packages("e1071")
library(e1071)
library(parallel)

cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores)

write.table(permutations(15),file = "D:/Rdata/boom/per.txt")


flist<-print(permutations(15))
ans1<-c(1,7,15,1,14,2,13,3,12,4,11,5,10,6,9,8)
ans2<-c(1,1,15,2,14,3,13,4,12,5,11,6,10,7,9,8)
ans3<-c(1,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
ans4<-c(1,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15)
ans5<-c(1,3,6,9,12,15,1,4,7,10,13,2,5,8,11,14)
ans6<-c(1,4,8,12,3,7,11,15,2,6,10,14,1,5,9,13)
ans7<-c(1,5,10,15,4,9,14,3,8,13,2,7,12,1,6,11)
ans8<-c(1,4,8,12,1,5,9,13,2,6,10,14,3,7,11,15)
ord<-ans4


compscore<-function(ord) {
  ans<-ord
  score<-1
  for (i in c(1:15)) {
    if (ans[i+1]>ans[i]) score<-score*(ans[i+1]-ans[i]) else score<-score/ans[i+1]
  }
  return(score)
}

compscore(ans8)

testres<-compscore(c(1,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15))

gc()
write.table(permutations(15),file = "D:/Rdata/boom/per.txt",row.names = F,col.names = F)

list<-matrix(nrow = 15,ncol = 15)
for (i in c(1:15)) {
  for (j in c(1:15)) {
    list[i,j]=j
  }
}

orgseq<-c(1:15)
score<-rep(1,time=15)
result<-1

for (i1 in orgseq) {
  list1<-orgseq[-which(orgseq==i1)]
  print(paste("i1=",i1,sep = ""))
  score[1]<-1*i1
  if (i1<3) {next}
  for (i2 in list1) {
    print(paste("i2=",i2,sep = ""))
    if ((i2>i1 & (i2-i1<2 | i2-i1>6))|(i2<i1 & i2>5)) {next}
    if (i2>i1) {score[2]<-score[1]*(i2-i1)} else {score[2]<-score[1]/i2}
    list2<-list1[-which(list1==i2)]
    for (i3 in list2) {
      if ((i3>i2 & (i3-i2<2 | i3-i2>6))|(i3<i2 & i3>5)) {next}
      if (i3>i2) {score[3]<-score[2]*(i3-i2)} else {score[3]<-score[2]/i3}
      list3<-list2[-which(list2==i3)]
      for (i4 in list3) {
        if ((i4>i3 & (i4-i3<2 | i4-i3>6))|(i4<i3 & i4>5)) {next}
        if (i4>i3) {score[4]<-score[3]*(i4-i3)} else {score[4]<-score[3]/i4}
        list4<-list3[-which(list3==i4)]
        for (i5 in list4) {
          if ((i5>i4 & (i5-i4<2 | i5-i4>6))|(i5<i4 & i5>5)) {next}
          if (i5>i4) {score[5]<-score[4]*(i5-i4)} else {score[5]<-score[4]/i5}
          list5<-list4[-which(list4==i5)]
          for (i6 in list5) {
            if ((i6>i5 & (i6-i5<2 | i6-i5>6))|(i6<i5 & i6>5)) {next}
            if (i6>i5) {score[6]<-score[5]*(i6-i5)} else {score[6]<-score[5]/i6}
            list6<-list5[-which(list5==i6)]
            for (i7 in list6) {
              if ((i7>i6 & (i7-i6<2 | i7-i6>6))|(i7<i6 & i7>5)) {next}
              if (i7>i6) {score[7]<-score[6]*(i7-i6)} else {score[7]<-score[6]/i7}
              list7<-list6[-which(list6==i7)]
              for (i8 in list7) {
                if ((i8>i7 & (i8-i7<2 | i8-i7>6))|(i8<i7 & i8>5)) {next}
                if (i8>i7) {score[8]<-score[7]*(i8-i7)} else {score[8]<-score[7]/i8}
                list8<-list7[-which(list7==i8)]
                for (i9 in list8) {
                  if ((i9>i8 & (i9-i8<2 | i9-i8>6))|(i9<i8 & i9>5)) {next}
                  if (i9>i8) {score[9]<-score[8]*(i9-i8)} else {score[9]<-score[8]/i9}
                  list9<-list8[-which(list8==i9)]
                  for (i10 in list9) {
                    if ((i10>i9 & (i10-i9<2 | i10-i9>6))|(i10<i9 & i10>5)) {next}
                    if (i10>i9) {score[10]<-score[9]*(i10-i9)} else {score[10]<-score[9]/i10}
                    list10<-list9[-which(list9==i10)]
                    for (i11 in list10) {
                      if ((i11>i10 & (i11-i10<2 | i11-i10>6))|(i11<i10 & i11>5)) {next}
                      if (i11>i10) {score[11]<-score[10]*(i11-i10)} else {score[11]<-score[10]/i11}
                      list11<-list10[-which(list10==i11)]
                      for (i12 in list11) {
                        if ((i12>i11 & (i12-i11<2 | i12-i11>6))|(i12<i11 & i12>5)) {next}
                        if (i12>i11) {score[12]<-score[11]*(i12-i11)} else {score[12]<-score[11]/i12}
                        list12<-list11[-which(list11==i12)]
                        for (i13 in list12) {
                          if ((i13>i12 & (i13-i12<2 | i13-i12>6))|(i13<i12 & i13>5)) {next}
                          if (i13>i12) {score[13]<-score[12]*(i13-i12)} else {score[13]<-score[12]/i13}
                          list13<-list12[-which(list12==i13)]
                          for (i14 in list13) {
                            if ((i14>i13 & (i14-i13<2 | i14-i13>6))|(i14<i13 & i14>5)) {next}
                            if (i14>i13) {score[14]<-score[13]*(i14-i13)} else {score[14]<-score[13]/i14}
                            list14<-list13[-which(list13==i14)]
                            for (i15 in list14) {
                              if ((i15>i14 & (i15-i14<2 | i5-i14>6))|(i15<i14 & i15>5)) {next}
                              if (i15>i14) {score[15]<-score[14]*(i15-i14)} else {score[15]<-score[14]/i15}
                              #if (sum(i1:i12<15)) print(c(1,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15))
                              if (result<score[15]) {
                                result<-score[15]
                                bestans<-c(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
                                }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
}
