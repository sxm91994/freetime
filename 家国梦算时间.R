multiplier=1000


jgmtime=function(tasksize,completed,income_OL,income_OFFL,return_value="None"){

est_ol_s=(tasksize-completed)/income_OL
est_offl_s=(tasksize-completed)/income_OFFL

#time all online

ol_h=est_ol_s%/%3600
ol_m=(est_ol_s%%3600)%/%60
ol_s=(est_ol_s%%60)%/%60

time_ol=paste(ol_h,"h",ol_m,"m",ol_s,"s",sep = "")

#tp_ol=time_ol+systime

#time all offline
offl_h=est_offl_s%/%3600
offl_m=(est_offl_s%%3600)%/%60
offl_s=(est_offl_s%%60)%/%60

time_offl=paste(offl_h,"h",offl_m,"m",offl_s,"s",sep = "")
#tp_offl=time_offl+systime

cat(paste("Estimated Completion Time:\n
          Fully online: ",time_ol,"\n
          Fully offline: ",time_offl,"\n"
          ,sep=""))
if (return_value=="ol") {return(est_ol_s)}
if (return_value=="offl") {return(est_offl_s)}
}


jgmtime(tasksize=416
             ,completed=112
             ,income_OL=36.6/(multiplier^1)
             ,income_OFFL=12.9/(multiplier^1)
             ,return_value = "ol"
)+Sys.time()
