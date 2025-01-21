source('~/R_func/ReadingData/Rtfunction.R')

trace=as.data.frame(read.csv( paste0("./",folder,"/COVID-19Piemonte-analysys-1.trace"), sep = ""))

trace$c_Lq_a0  <- trace$c_SW_a0 + trace$c_Lq_a0 
# trace$c_Lu_a0  <- trace$c_Lu_a0 - trace$c_SW_a0 
trace$c_Lq_a1  <- trace$c_SW_a1 + trace$c_Lq_a1 
# trace$c_Lu_a1  <- trace$c_Lu_a1 - trace$c_SW_a1
trace$c_Lq_a2  <- trace$c_SW_a2 + trace$c_Lq_a2 
# trace$c_Lu_a2  <- trace$c_Lu_a2 - trace$c_SW_a2

age_classes<-3
AgesNames=c("0-19 years old","20-69 years old","70++ years old","Comulative")

l2<-list("c_Lu_a0","c_Lq_a0","c_Lh_a0","c_Lu_a1","c_Lq_a1","c_Lh_a1","c_Lu_a2","c_Lq_a2","c_Lh_a2",c(paste0("c_Lu_a",0:2)), c( paste0("c_Lq_a",0:2)),c( paste0("c_Lh_a",0:2) ) )


### deleting if there are points beyond te final time
n_sim<-unique(table(trace$Time))

time_to_delete<-unique(trace$Time)[which(table(trace$Time)!=max(n_sim))]
if(length(time_to_delete)!=0) trace<-trace[-which(trace$Time%in%time_to_delete),]
n_sim<-max(n_sim)
####################

trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time)) )
y_names <-names(trace)

data<-lapply(1:length(l2), function(ii){
  l2[[ii]]->ltemp
  ### fixed the age and symptom class, rows= each simulation col=times
  sim_traces_tmp<-sapply( unique(trace$Time) , function(k){
    if(length(ltemp)==1){
      trace[which(trace$Time==k),ltemp]}
    else{rowSums( trace[which(trace$Time==k),ltemp] ) }
  }) 
  
  m<-apply(sim_traces_tmp,2,median)
  
  Idiff=c(m[1], diff(m,lag = 1) )
  data.frame(Time = unique(trace$Time) ,I=m,Idiff=Idiff)
  
})

dataGG<-do.call("rbind",data)

dataGG$Sympt =ordered( rep(rep(c("Iu","Iq","Ih"),each= length(unique(trace$Time)) ) ,(age_classes+1)), levels = c("Ih","Iq","Iu")[3:1])
dataGG$Ages =rep(AgesNames,each= length(unique(trace$Time)) *3)

Iq<-dataGG[which(dataGG$Ages=="Comulative" & dataGG$Sympt=="Iq"),]$Idiff
Ih<-dataGG[which(dataGG$Ages=="Comulative" & dataGG$Sympt=="Ih"),]$Idiff

Rt_list<-Rt_calculation(Iq+Ih)

Rt_list$Rt$Rt_oneGamma
Rt_list$Rt$Rt_familyGamma

ggsave(plot = Rt_list$Rt$Rt_familyGamma,filename = paste("Plot/Rt_",nameplot,".pdf"),
       dpi = 400, width = 16, height = 8,device = "pdf")
