id=1
library(ggplot2)
library(cowplot)

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

hyst <- reference[which(reference$Time >= "2020-02-24" ),]

Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))) 

y_names <-names(trace)
# Get the indexes of the places to consider

a0=c("c_Lq_a0","c_Lh_a0")
a1=c("c_Lq_a1","c_Lh_a1")
a2=c("c_Lq_a2","c_Lh_a2")
d=c("d_a1","d_a2")
l<-list(a0,a1,a2,c(a0,a1,a2),"d_a1","d_a2",d)

categ=age_classes+1

ltemp<-l[[categ]]
  
  # Compute the increments in the number of infects year by year
  sim_traces_tmp<-lapply(0:(n_sim-1),
                         function(k){
                           
                           out<-trace[(1+k*(length(Time))):(length(Time)+k*length(Time)),ltemp]
                           if(length(ltemp)>1)
                           {
                             infected<-rowSums(out)
                           }else{
                             infected<-out
                           }
                           
                           c(infected,1)
                           
                         }
  )
  
  sim_traces_tmp2 <- do.call("rbind", sim_traces_tmp)
  colnames(sim_traces_tmp2) <- c(paste0(Time),"ID")
  
  
  
  # Give an unique id to each simulation
  if(length(l[[categ]])>1){
    sim_traces<-data.frame(Time=rep(Time,n_sim),infected=rowSums(trace[,ltemp]),IDconf=rep(1,each= length(Time)*n_sim ),IDsim=rep(1:(n_sim),each= length(Time)) )
  }else{
    sim_traces<-data.frame(Time=rep(Time,n_sim),infected=trace[,ltemp],IDconf=rep(1,each= length(Time)*n_sim ),IDsim=rep(1:(n_sim),each= length(Time)) )
    
  }
  
  # PLOT 1: all traces of the same experiment, their mean value and the hystorical data
  # Comupte mean, median and the standard deviation
  stats<-lapply(id,function(k)
  {
    dat<-sim_traces_tmp2[which(sim_traces_tmp2[,"ID"]==k),-length(sim_traces_tmp2[1,])]
    mean<-apply(dat,2,mean)
    median <- apply(dat,2,median)
    sd<-apply(dat,2,sd)
    # Compute the "confidence interval" for the mean
    # up<-mean+(qnorm(.99))*sd/sqrt(n_sim)
    # lp<-mean-(qnorm(.99))*sd/sqrt(n_sim)
    up<-apply(dat,2,quantile,prob=.75)
    lp<-apply(dat,2,quantile,prob=.25)
    
    Mediandiff= c(median[1], diff(median,lag = 1) )
    
    data.frame(days = Time,median=median,Mediandiff=Mediandiff,mean=mean,up=up,lp=lp,IDconf=paste(k))
  }
  )
  
  stats<-do.call("rbind", stats)
  Meanarea<-t(sapply(unique(stats$days), function(x){
    minMean<- min(stats$mean[which(stats$days==x)])
    maxMean<-max(stats$mean[which(stats$days==x)])
    minSD<- min(stats$lp[which(stats$days==x)])
    maxSD<-max(stats$up[which(stats$days==x)])
    c(x,minMean,maxMean,minSD,maxSD)
  }))
  Meanarea<-as.data.frame(Meanarea)
  colnames(Meanarea)<-c("time","min","max","lp","up")
  
  Meanarea$time<-Time
  
plend<-ggplot( )+
     geom_line(data = sim_traces,aes(x=Time,y=infected,group=IDsim,col="Simulated data"),alpha=.7)+
     geom_line(data = hyst, aes(x=Time,y=hyst[,categ+1], col="Survey data")) +
     geom_line(data = stats, aes(x=days,y=median, col="Median"))+
  #geom_line(data = stats, aes(x=days,y=median, col="Mean"))+
     geom_ribbon(data=Meanarea, 
                 aes(x=time,ymin=lp,ymax=up,col="Standard Deviation"), fill="green", alpha=.1,linetype="dashed",show.legend=FALSE)+
     scale_color_manual("",values=c("Survey data"="red","Median"="darkgreen","Simulated data"="grey","Standard Deviation"="green"),
                        labels=c("Surveillance data","Median","Simulated data","25th-75th quantiles"),
                        breaks=c("Survey data","Median","Simulated data","Standard Deviation"))+
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
     labs(x="Days", y="Comulative infected cases",title = "" )+
     geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
     annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
     geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
     annotate("text",x=as.Date("2020-03-09"), label="\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
     geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
     annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
     scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
     scale_y_continuous(breaks= scales::pretty_breaks(n = 8))
   
pl22<- ggplot( )+
    geom_line(data = sim_traces[which(sim_traces$Time <= max(hyst$Time)),],aes(x=Time,y=infected,group=IDsim,col="Simulated data"),alpha=.7)+
    geom_line(data = hyst, aes(x=Time,y=hyst[,categ+1], col="Survey data")) +
    geom_line(data = stats[which(stats$days <= max(hyst$Time)),], aes(x=days,y=median, col="Median"))+
    geom_ribbon(data=Meanarea[which(Meanarea$time <= max(hyst$Time)),], 
                aes(x=time,ymin=lp,ymax=up,col="Standard Deviation"), fill="green", alpha=.1,linetype="dashed",show.legend=FALSE)+
    scale_color_manual("",values=c("Survey data"="red","Median"="darkgreen","Simulated data"="grey","Standard Deviation"="green"),
                       labels=c("Surveillance data","Median","Simulated data","25th-75th quantiles"),
                       breaks=c("Survey data","Median","Simulated data","Standard Deviation"))+
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
    labs(x="Days", y="Comulative infected cases",title = "" )+
    geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
    annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
    geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
    annotate("text",x=as.Date("2020-03-09"), label="\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
    geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
    annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
    scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
    scale_y_continuous(breaks= scales::pretty_breaks(n = 8))

ggsave(plot = plend,filename = paste0("Plot/StochSimulation",nameplot,".pdf"),
       dpi = 400, width = 20, height = 8,device = "pdf")

