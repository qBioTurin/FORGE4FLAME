
id=1
library(ggplot2)
library(cowplot)

# If median = T then the median will be plotted, otherwise the mean
medianTrue = T

trace=as.data.frame(read.csv( paste0("./results_model_analysisScenario1/COVID-19Piemonte-analysis-1.trace"), sep = ""))

 trace$c_Lq_a0  <- trace$c_SW_a0 + trace$c_Lq_a0 
# trace$c_Lu_a0  <- trace$c_Lu_a0 - trace$c_SW_a0 
 trace$c_Lq_a1  <- trace$c_SW_a1 + trace$c_Lq_a1 
# trace$c_Lu_a1  <- trace$c_Lu_a1 - trace$c_SW_a1
 trace$c_Lq_a2  <- trace$c_SW_a2 + trace$c_Lq_a2 
# trace$c_Lu_a2  <- trace$c_Lu_a2 - trace$c_SW_a2

### deleting if there are points beyond te final time
n_sim<-unique(table(trace$Time))

time_to_delete<-unique(trace$Time)[which(table(trace$Time)!=max(n_sim))]
if(length(time_to_delete)!=0) trace<-trace[-which(trace$Time%in%time_to_delete),]
n_sim<-max(n_sim)
####################

AgesNames=c("0-19 years old","20-69 years old","70++ years old","Comulative")

l2<-list("c_Lu_a0","c_Lq_a0","c_Lh_a0","c_Lu_a1","c_Lq_a1","c_Lh_a1","c_Lu_a2","c_Lq_a2","c_Lh_a2",c(paste0("c_Lu_a",0:2)), c( paste0("c_Lq_a",0:2)),c( paste0("c_Lh_a",0:2) ) )


trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time)) )

#### Reference:
reference <- as.data.frame(t(read.csv("input/reference.csv",
                                      header = FALSE,
                                      sep = "")))


age_classes<-3

# updated at 16/04 with tree age classes
p<-.252+.305+.24
death_percentages<-c(1-p,p)

Death_ages <-as.data.frame(matrix((reference[,age_classes+1]),ncol=1) %*% matrix(death_percentages,ncol=(age_classes-1)) )


reference<- data.frame(Time=trace$Time[0:length(reference$V1)], reference[,1:age_classes],rowSums(reference[,1:age_classes]),Death_ages,reference[,age_classes+1])

reference <- reference[which(reference$Time >= "2020-02-24" ),]

reference_big_hist<- lapply(1:(age_classes+1), function(j){
  data.frame(Time=reference$Time,I=reference[,j+1],Idiff=c(reference[1,j+1], diff(reference[,j+1],lag = 1) ),Ages=AgesNames[j])
})
reference_big_hist<-do.call("rbind",reference_big_hist) 


Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))) 

y_names <-names(trace)

data<-lapply(1:length(l2), function(i){
  l2[[i]]->ltemp
  ### fixed the age and symptom class, rows= each simulation col=times
  sim_traces_tmp<-sapply(Time, function(k){
    if(length(ltemp)==1){
      trace[which(trace$Time==k),ltemp]}
    else{rowSums( trace[which(trace$Time==k),ltemp] ) }
  }) 
  
  if(medianTrue) m<-apply(sim_traces_tmp,2,median)
  else  m<-apply(sim_traces_tmp,2,mean)
  
  
  Idiff=c(m[1], diff(m,lag = 1) )
  data.frame(Time=Time,I=m,Idiff=Idiff)
  
})

dataGG<-do.call("rbind",data)

dataGG$Sympt =ordered( rep(rep(c("Iu","Iq","Ih"),each=length(Time)),(age_classes+1)), levels = c("Ih","Iq","Iu")[3:1])
dataGG$Ages =rep(AgesNames,each=length(Time)*3)

 pl<- ggplot(data=dataGG[which(dataGG$Ages == "Comulative"),],
             aes(x=Time,y=Idiff) )+
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist[which(reference_big_hist$Ages == "Comulative"),],
            aes(x=Time,y=Idiff,col="red"),size=1.1)+
   scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red"),
                       limits = c("Ih","Iq","Iu","red"),
                       labels = c("Ih","Iq","Iu","Surveillance\n data") )+
    scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA),
                      limits = c("Ih","Iq","Iu","red"),
                      labels = c("Ih","Iq","Iu","Surveillance\n data") )+
  facet_wrap(~ Ages, scales="free",ncol = 2)+
   theme(axis.text=element_text(size = 15, hjust = 0.5),
         axis.text.x=element_text(angle=+90),
         axis.title=element_text(size=18,face="bold"),
         axis.line = element_line(colour="black"),
         plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
         legend.text=element_text(size=14),
         legend.position=c(.1,.75),
         legend.title=element_blank(),
         legend.key=element_blank(),
         legend.key.size = unit(.9, "cm"),
         legend.key.width = unit(.9,"cm"),
         panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA),
         plot.margin=unit(c(5,5,5,5),"mm"), 
         strip.background = element_blank(),
         strip.text.x = element_blank() )+
  labs(x="Days", y="New Daily Infected cases" )+
   geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
   annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
   geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
   annotate("text",x=as.Date("2020-03-09"), label="\n\n\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
   geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
   annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
   scale_x_date( breaks = seq(as.Date("2020/02/21"), length.out = length(unique(trace$Time)), by = "week"), date_labels = "%b-%d")+
   scale_y_continuous(breaks= scales::pretty_breaks(n = 8))
 

 
 ##### plot deaths
 
 
 ldeath<-list("d_a1","d_a2",c("d_a1","d_a2"))
 
 
 reference_big_hist_DEATH<- lapply(1:3, function(j){
   data.frame(Time=reference$Time,I=reference[,j+5],Ddiff=c(reference[1,j+5], diff(reference[,j+5],lag = 1) ),Ages=AgesNames[1+j])
 })
 reference_big_hist_DEATH <- do.call("rbind",reference_big_hist_DEATH) 
 
 
 data<-lapply(1:length(ldeath), function(i){
   ldeath[[i]]->ltemp
   ### fixed the age and symptom class, rows= each simulation col=times
   sim_traces_tmp<-sapply(Time, function(k){
     if(length(ltemp)==1){
       trace[which(trace$Time==k),ltemp]}
     else{rowSums( trace[which(trace$Time==k),ltemp] ) }
   }) 
   
   if(medianTrue) m<-apply(sim_traces_tmp,2,median)
   else  m<-apply(sim_traces_tmp,2,mean)
   
   
   Ddiff=c(m[1], diff(m,lag = 1) )
   data.frame(Time=Time,D=m,Ddiff=Ddiff)
   
 })
 
 DeathGG<-do.call("rbind",data)
 
 DeathGG$Ages =rep(AgesNames[-1],each=length(Time))
 
 plD<-ggplot(data=DeathGG[which(DeathGG$Ages == "Comulative"),],
             aes(x=Time,y=D,fill="Deaths") )+
   geom_bar(stat="identity",alpha=.5)+
   geom_line(data=reference_big_hist_DEATH[which(reference_big_hist_DEATH$Ages == "Comulative"),],
             aes(x=Time,y=I,col="red"),size=1.1)+
   scale_colour_manual("" , values= c("Deaths"=NA,"red"="red"),
                       limits = c("Deaths","red"),
                       labels = c("Deaths","Surveillance\n data") )+
   scale_fill_manual("" , values= c("Deaths"="#499894", "red"=NA),
                     limits = c("Deaths","red"),
                     labels = c("Deaths","Surveillance\n data") )+
   # geom_line(data=reference_big_hist[which(reference_big_hist$Ages == "Comulative"),],
   #           aes(x=Time,y=Idiff,col="red"),size=1.1)+
   # scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red"),
   #                     limits = c("Ih","Iq","Iu","red"),
   #                     labels = c("Ih","Iq","Iu","Surveillance\n data") )+
   facet_wrap(~ Ages, scales="free",ncol = 2)+
   theme(axis.text=element_text(size = 15, hjust = 0.5),
         axis.text.x=element_text(angle=+90),
         axis.title=element_text(size=18,face="bold"),
         axis.line = element_line(colour="black"),
         plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
         legend.text=element_text(size=14),
         legend.position=c(.1,.75),
         legend.title=element_blank(),
         legend.key=element_blank(),
         legend.key.size = unit(.9, "cm"),
         legend.key.width = unit(.9,"cm"),
         panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA),
         plot.margin=unit(c(5,5,5,5),"mm"), 
         strip.background = element_blank(),
         strip.text.x = element_blank() )+
   labs(x="Days", y="Comulative number of deaths" )+
   geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
   annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
   geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
   annotate("text",x=as.Date("2020-03-09"), label="\n\n\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
   geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
   annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
   scale_x_date( breaks = seq(as.Date("2020/02/21"), length.out = length(unique(trace$Time)), by = "week"), date_labels = "%b-%d")+
   scale_y_continuous(breaks= scales::pretty_breaks(n = 8))
 
 
 ################## detection 
 ldetect<-list("c_SW_a0","c_SW_a1","c_SW_a2",c("c_SW_a0","c_SW_a1","c_SW_a2"))
 
 
 data<-lapply(1:length(ldetect), function(i){
   ldetect[[i]]->ltemp
   ### fixed the age and symptom class, rows= each simulation col=times
   sim_traces_tmp<-sapply(Time, function(k){
     if(length(ltemp)==1){
       trace[which(trace$Time==k),ltemp]}
     else{rowSums( trace[which(trace$Time==k),ltemp] ) }
   }) 
   
   if(medianTrue) m<-apply(sim_traces_tmp,2,median)
   else  m<-apply(sim_traces_tmp,2,mean)
   
   
   Ddiff=c(m[1], diff(m,lag = 1) )
   data.frame(Time=Time,D=m,Ddiff=Ddiff)
   
 })
 
 Detect<-do.call("rbind",data)
 
 Detect$Ages =rep(AgesNames,each=length(Time))
 
 plDet<-ggplot(data=Detect[which(Detect$Ages != "Comulative"),],
             aes(x=Time,y=Ddiff,col=Ages) )+
   geom_line()+
   theme(axis.text=element_text(size = 15, hjust = 0.5),
         axis.text.x=element_text(angle=+90),
         axis.title=element_text(size=18,face="bold"),
         axis.line = element_line(colour="black"),
         plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
         legend.text=element_text(size=14),
         legend.position=c(.1,.75),
         legend.key=element_blank(),
         legend.key.size = unit(.9, "cm"),
         legend.key.width = unit(.9,"cm"),
         panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA),
         plot.margin=unit(c(5,5,5,5),"mm"), 
         strip.background = element_blank(),
         strip.text.x = element_blank() )+
   labs(x="Days", y="Daily number of new detected" )+
   scale_y_continuous(breaks= scales::pretty_breaks(n = 8))
 
 
 pl1<-pl + geom_line(data=Detect[which(Detect$Ages == "Comulative"),], 
                aes(x=Time,y=Ddiff, col= "Detection"),size=1.1 )+
   scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red", "Detection"="purple"),
                       limits = c("Ih","Iq","Iu","red", "Detection"),
                       labels = c("Ih","Iq","Iu","Surveillance\n data", "Detection") )+
   scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA,"Detection"=NA),
                     limits = c("Ih","Iq","Iu","red","Detection"),
                     labels = c("Ih","Iq","Iu","Surveillance\n data","Detection") )
 
 #### save the plots

 #pl <- pl + ylim(0,1900)
 #pl1 <- pl1 + ylim(0,1900)
 #plD <- plD + ylim(0,12000)
 
 
 if(medianTrue){
   ggsave(plot = plD,filename = paste0("Plot/DailyDeath",nameplot,".pdf"),
          dpi = 400, width = 20, height = 8,device = "pdf")
 }else{  ggsave(plot = plD,filename = paste0("Plot/DailyDeathMean",nameplot,".pdf"),
                dpi = 400,width = 20, height = 8,device = "pdf") }
 
 
 if(medianTrue){
   ggsave(plot = pl,filename = paste0("Plot/StochMedianDiff",nameplot,".pdf"),
          dpi = 400, width = 20, height = 8,device = "pdf")
   ggsave(plot = pl1,filename = paste0("Plot/StochMedianDiffWithDetection",nameplot,".pdf"),
          dpi = 400, width = 20, height = 8,device = "pdf")
 }else{  ggsave(plot = pl,filename = paste0("Plot/StochMeanDiff",nameplot,".pdf"),
                dpi = 400,width = 20, height = 8,device = "pdf") }
 
   ggsave(plot = plDet,filename = paste0("Plot/DailyDetection",nameplot,".pdf"),
          dpi = 400, width = 20, height = 8,device = "pdf")
 