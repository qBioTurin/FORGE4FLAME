library(cowplot)
library(ggplot2)
library(ggthemes)

trace=as.data.frame(read.csv( "./COVID-19Piemonte_analysis/COVID-19Piemonte-analysis-1.trace", sep = ""))
#trace=as.data.frame(read.csv( "./results_model_calibration/COVID-19Piemonte-calibration-49367.trace", sep = ""))
trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(trace$Time))
reference <- as.data.frame(t(read.csv("input/reference.csv",
                                      header = FALSE,
                                      sep = "")))


trace$c_Lq_a2  <- trace$c_SW_a2 + trace$c_Lq_a2 
# trace$c_Lu_a2  <- trace$c_Lu_a2 - trace$c_SW_a2 


age_classes<-3

# updated at 16/04 with tree age classes
p<-.31+.405+.12
death_percentages<-c(1-p,p)

Death_ages <-as.data.frame(matrix((reference[,age_classes+1]),ncol=1) %*% matrix(death_percentages,ncol=(age_classes-1)) )


reference<- data.frame(Time=trace$Time[0:length(reference$V1)], reference[,1:age_classes],rowSums(reference[,1:age_classes]),Death_ages,reference[,age_classes+1])


titleList<-c("Infects 00-19 years old","Infects 20-69 years old","Infects 70++ years old","Infects all ages",
             "Deaths 20-69 years old","Deaths 70++ years old","Deaths all ages")
ylabel<- c(paste0("Total cases in a",0:(age_classes-1)),"Total cases",paste0("Total deaths in a",0:(age_classes-1)),"Total deaths")

a0=c("c_Lu_a0","c_Lq_a0","c_Lh_a0")
a1=c("c_Lu_a1","c_Lq_a1","c_Lh_a1")
a2=c("c_Lu_a2","c_Lq_a2","c_Lh_a2")
d=c("d_a1","d_a2")
l<-list(a0,a1,a2,c(a0,a1,a2),"d_a1","d_a2",d)
ldetect<-list("c_SW_a0","c_SW_a1","c_SW_a2",c("c_SW_a0","c_SW_a1","c_SW_a2"))

################################################
AgesNames=c("0-19 years old","20-69 years old","70++ years old","Comulative")

reference_big_hist<- lapply(1:(age_classes+1), function(j){
  data.frame(Time=reference$Time,I=reference[,j+1],Ages=AgesNames[j])
})
reference_big_hist<-do.call("rbind",reference_big_hist)  

l2<-list("c_Lu_a0","c_Lq_a0","c_Lh_a0","c_Lu_a1","c_Lq_a1","c_Lh_a1","c_Lu_a2","c_Lq_a2","c_Lh_a2",c(paste0("c_Lu_a",0:(age_classes-1))), c( paste0("c_Lq_a",0:(age_classes-1))),c( paste0("c_Lh_a",0:(age_classes-1)) ) )

Idiff=lapply(1:length(l2), function(i){
  
  if(length(l2[[i]])>1){
    Trace_plot<-rowSums(trace[, l2[[i]] ])
  }else{
    Trace_plot<-trace[, l2[[i]] ] 
  }
  
  c(Trace_plot[1], diff(Trace_plot,lag = 1) )
})
Idiff=unlist(Idiff)


Trace_big_hist <- data.frame( Time = rep(trace$Time, length(l[[(age_classes+1)]])+3 ),
                            I = c(unlist(trace[, l[[(age_classes+1)]] ]), rowSums(trace[, paste0("c_Lu_a",0:(age_classes-1))] ),rowSums(trace[, paste0("c_Lq_a",0:(age_classes-1))] ),rowSums(trace[, paste0("c_Lh_a",0:(age_classes-1))] )),
                            Idiff = Idiff,
                            Sympt=ordered( rep(rep(c("Iu","Iq","Ih"),each=length(trace$Time)),(age_classes+1)), levels = c("Ih","Iq","Iu")[3:1]),
                            Ages=rep(AgesNames,each=length(trace$Time)*3) )


Detect <- data.frame( Time = rep(trace$Time, length(ldetect) ), 
                      Det = c(unlist(trace[, ldetect[[4]] ]), rowSums(trace[, ldetect[[4]] ] ) ),
                      Ages=rep(AgesNames,each=length(trace$Time)) )
###################################

BIGHIST<- ggplot(data=Trace_big_hist,
                aes(x=Time,y=I) )+
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist,
            aes(x=Time,y=I,
                col="red"),size=1.1 ) +
  geom_line(data=Detect,
                aes(x=Time,y=Det , col= "Detection"),size=1.1 )+
  scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red", "Detection"="purple"),
                      limits = c("Ih","Iq","Iu","red", "Detection"),
                      labels = c("Ih","Iq","Iu","Surveillance", "Detection") )+
  scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA,"Detection"=NA),
                    limits = c("Ih","Iq","Iu","red","Detection"),
                    labels = c("Ih","Iq","Iu","Surveillance","Detection") )+
  facet_wrap(~ Ages, scales="free",ncol = 2)+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position="bottom",
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold",size = 15))+
  labs(x="Days", y="Comulative infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n1st public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n\n\n2nd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n3rd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))


ggsave(plot = BIGHIST,filename = "Plot/InfectsHistALL.pdf",
       dpi = 400, width = 35, height = 15,device = "pdf")

HIST<- ggplot(data=Trace_big_hist[which(Trace_big_hist$Ages == "Comulative"),],
              aes(x=Time,y=I) )+
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist[which(reference_big_hist$Ages == "Comulative" & reference_big_hist$Time >= "2020-02-24" ),],
            aes(x=Time,y=I,
                col="red"),size=1.1) +
  geom_line(data=Detect[which(Detect$Ages == "Comulative"),],
            aes(x=Time,y=Det, col= "Detection"),size=1.1 )+
  scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red", "Detection"="purple"),
                      limits = c("Ih","Iq","Iu","red", "Detection"),
                      labels = c("Ih","Iq","Iu","Surveillance", "Detection") )+
  scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA,"Detection"=NA),
                    limits = c("Ih","Iq","Iu","red","Detection"),
                    labels = c("Ih","Iq","Iu","Surveillance","Detection") )+
  facet_wrap(~ Ages, scales="free")+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position=c(.07,.75),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Comulative infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n1st public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n\n\n2nd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n3rd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))


############################################################################
## DEATHS

reference_big_hist_d<- lapply(1:(age_classes), function(j){
  data.frame(Time=reference$Time,D=reference[,j+(age_classes+2)],Ages=AgesNames[j+1])
})
reference_big_hist_d<-do.call("rbind",reference_big_hist_d)  

Trace_big_histD<-data.frame( Time = rep(trace$Time, length(d)+1 ),
                             D = c(unlist(trace[, d ]), rowSums(trace[, d] ) ),
                             Ages=rep(AgesNames[-1],each=length(trace$Time)) )


BIGHIST_d<-ggplot(data=Trace_big_histD,
                  aes(x=Time,y=D) )+
  geom_bar( aes(fill="Deaths",col="Deaths" ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist_d,
            aes(x=Time,y=D,col="red")
            )

BIGHIST_d<-BIGHIST_d+
  scale_colour_manual("" , values= c("Deaths"=NA,"red"="red"),
                      limits = c("Deaths","red"),
                      labels = c("D","Surveillance\n data") )+
  scale_fill_manual("" , values= c("Deaths"="#499894", "red"=NA),
                    limits = c("Deaths","red"),
                    labels = c("D","Surveillance\n data") )+
  facet_wrap(~ Ages, scales="free",ncol = 2)+
  theme(axis.text=element_text(size = rel(1.2), hjust = 0.5),
        axis.title=element_text(size=15,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20,face="bold"),
        legend.position="right",
        legend.key.size = unit(1.3, "cm"),
        legend.key.width = unit(1.3,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold",size = 15))+
  labs(x="Days", y= "Deaths",title = "" ) +  theme(legend.position = "bottom") 

ggsave(plot = BIGHIST_d,filename = "Plot/DeathsHistALL.pdf",
       dpi = 400, width = 30, height = 10,device = "pdf")



HIST_d<-ggplot(data=Trace_big_histD[which(Trace_big_histD$Ages == "Comulative"),],
               aes(x=Time,y=D) )+
  geom_bar( aes(fill="Deaths",col="Deaths" ),
            stat="identity",alpha=.9)+
  geom_line(data=reference_big_hist_d[which(reference_big_hist_d$Ages == "Comulative"& reference_big_hist_d$Time >= "2020-02-24"),],
            aes(x=Time,y=D,
                col="red"))+
  facet_wrap(~ Ages, scales="free")+
  scale_colour_manual("" , values= c("Deaths"=NA,"red"="red"),
                      limits = c("Deaths","red"),
                      labels = c("D","Surveillance\n data") )+
  scale_fill_manual("" , values= c("Deaths"="#499894", "red"=NA),
                    limits = c("Deaths","red"),
                    labels = c("D","Surveillance\n data") )+
  facet_wrap(~ Ages, scales="free")+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position=c(.07,.8),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Deaths" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n1st public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n\n\n2nd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n3rd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))


#######################################
Traces <- data.frame( Time = rep(trace$Time,length(AgesNames) ),
                      I = c(rowSums(trace[, c("c_Lq_a0","c_Lh_a0")] ),rowSums(trace[, c("c_Lq_a1","c_Lh_a1")] ),
                            rowSums(trace[, c("c_Lq_a2","c_Lh_a2")] ),rowSums(trace[, c(paste0("c_Lq_a",0:(age_classes-1)),paste0("c_Lh_a",0:(age_classes-1)))] )),
                      Ages=rep(AgesNames,each=length(trace$Time)) )

plLINES<- ggplot(data=Trace_big_hist,
                 aes(x=Time,y=I) )+
  geom_line(data=Traces,
            aes(x=Time,y=I))+
  geom_line(data=reference_big_hist,
            aes(x=Time,y=I,
                col="red")) +
  facet_wrap(~ Ages, scales="free",ncol = 2)+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position=c(.07,.8),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n1st public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n\n\n2nd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n3rd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))




ggsave(plot = plot_grid(plotlist = list(HIST,HIST_d),labels = c("A","B"),nrow=2,label_size = 20 ),filename = "Plot/Comulatives.pdf",
       dpi = 400, width = 15, height = 15,device = "pdf")


ggsave(plot = plLINES,filename = "Plot/lines.pdf",
       dpi = 400, width = 30, height = 10,device = "pdf")


plLINESzoomed<- ggplot( )+
  geom_line(data=Traces[which(Traces$Time<"2020-03-09"),],
            aes(x=Time,y=I))+
  geom_line(data=reference_big_hist[which(reference_big_hist$Time<"2020-03-09"),],
            aes(x=Time,y=I,
                col="red")) +
  facet_wrap(~ Ages, scales="free",ncol = 2)+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.position=c(.07,.8),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(0,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n1st public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n\n\n2nd public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date(date_breaks = "week" , date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))

ggsave(plot = plLINESzoomed,filename = "Plot/lineszoom.pdf",
       dpi = 400, width = 30, height = 10,device = "pdf")
