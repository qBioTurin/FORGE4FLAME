id=1
library(ggplot2)
library(cowplot)
library(patchwork)


age_classes<-3

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(6)[c(2,3,4)]

cols = c("#00798c", "#edae49", "#66a182")
##### reading traces:
trace55=as.data.frame(read.csv( "./results_model_analysisStochWorst/COVID-19Piemonte-analysys-1.trace", sep = "") )
trace81=as.data.frame(read.csv( "./results_model_analysisStochEstimated/COVID-19Piemonte-analysys-1.trace", sep = "") )
trace95=as.data.frame(read.csv( "./results_model_analysisStochBest/COVID-19Piemonte-analysys-1.trace", sep = "") )

trace55$alpha<-"Worst"
trace81$alpha<-"Estimated"
trace95$alpha<-"Best"
alphatype<-c("Worst","Estimated","Best")

trace <- rbind(trace55,trace81,trace95)
### deleting if there are points beyond te final time
n_sim<-unique(table(trace$Time))

time_to_delete<-unique(trace$Time)[which(table(trace$Time)!=max(n_sim))]
if(length(time_to_delete)!=0) trace<-trace[-which(trace$Time%in%time_to_delete),]
n_sim<-max(n_sim)
####################

trace$Time<-seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time)) )

Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))) 

y_names <-names(trace)
# Get the indexes of the places to consider


a0=c("c_Lq_a0","c_Lh_a0")
a1=c("c_Lq_a1","c_Lh_a1")
a2=c("c_Lq_a2","c_Lh_a2")

d=c("d_a1","d_a2")
l<-list(a0,a1,a2,c(a0,a1,a2),"d_a1","d_a2",d)
titleList<-c("Infects 00-19 years old","Infects 20-69 years old","Infects 70++ years old","Infects all ages",
             "Deaths 20-69 years old","Deaths 70++ years old","Deaths all ages")
ylabel<- c(paste0("Total cases in a",0:(age_classes-1)),"Total cases",paste0("Total deaths in a",0:(age_classes-1)),"Total deaths")

AgesNames=c("0-19 years old","20-69 years old","70++ years old","Comulative")



categ=age_classes+1

ltemp<-l[[categ]]

# Compute the increments in the number of infects year by year
sim_traces_tmp<-lapply(0:((n_sim) -1),
                       function(k){
                         out<-trace[(1+k*(length(Time))):(length(Time)+k*length(Time)),ltemp]
                         if(length(ltemp)>1)
                         {
                           infected<-rowSums(out)
                         }else{
                           infected<-out
                         }
                         data.frame(Time= Time,
                                    I = infected, 
                                    Alpha = trace$alpha[(1+k*(length(Time))):(length(Time)+k*length(Time))] )
                         
                       }
)


sim_traces <- do.call("rbind", sim_traces_tmp)

sim_traces[which(sim_traces$Time == max(sim_traces$Time)),]->ggplotdf
myFun <- function(x) {
  c(median(x))
}

medians<-tapply(ggplotdf$I, ggplotdf$Alpha, myFun)
medians<-unlist(medians)

range(sim_traces$I)

y_range <- c(0,95000)
  
pl<- ggplot(data = sim_traces[which(sim_traces$Time == max(sim_traces$Time)),],aes(x=I)  )+
  geom_histogram(aes(y=..density.. , colour = Alpha),position="identity", fill = "white",bins = sqrt(n_sim))+
  geom_density(aes(fill=Alpha,group=Alpha),alpha=.3,position="identity")+
  scale_fill_manual("Strength of the third \ngovernmental action",values=cols)+
  scale_colour_manual("Strength of the third \ngovernmental action",values=cols)+
  geom_vline(xintercept=medians, colour="darkgreen", linetype = "longdash" )+
  theme(axis.text=element_text(size = 13, hjust = 0.5),
        axis.title=element_text(size=15,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=12),
        legend.position=c(.8,.8),
        legend.title=element_text(size=16,face="bold"),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(y="Density\n\n\n", x="",title = "" )+
scale_x_continuous(limits = y_range, expand = expand_scale(mult = 0.1))+
  coord_flip()

ggsave(plot = pl, filename = "./Plot/StochFutComDistribution.pdf",
       dpi = 400, width = 12, height =8,device = "pdf")

################################################
##################################### plot aree with different alpha

n_conf=length(alphatype)
n_sim=n_sim/n_conf
sim_traces_tmp<-lapply(0:((n_sim)*n_conf-1),
                       function(k){
                         
                         out<-trace[(1+k*(length(Time))):(length(Time)+k*length(Time)),ltemp]
                         if(length(ltemp)>1)
                         {
                           infected<-rowSums(out)
                         }else{
                           infected<-out
                         }
                         
                         c(infected,as.integer(k/n_sim)+1)
                         
                       }
)


sim_traces_tmp2 <- do.call("rbind", sim_traces_tmp)
colnames(sim_traces_tmp2) <- c(paste0(Time),"ID")

stats<-lapply(1:3,function(k)
{
  dat<-sim_traces_tmp2[which( sim_traces_tmp2[,"ID"] ==k),-length(sim_traces_tmp2[1,])]
  mean<-apply(dat,2,mean)
  median <- apply(dat,2,median)
  sd<-apply(dat,2,sd)
  # Compute the "confidence interval" for the mean
  # up<-mean+(qnorm(.99))*sd/sqrt(n_sim)
  # lp<-mean-(qnorm(.99))*sd/sqrt(n_sim)
  up<-apply(dat,2,max)
  lp<-apply(dat,2,min)
  
  Mediandiff= c(median[1], diff(median,lag = 1) )
  
  data.frame(days = Time,median=median,Mediandiff=Mediandiff,mean=mean,up=up,lp=lp,Alpha=alphatype[k])
}
)

stats<-do.call("rbind", stats)




pl_aree<-ggplot( data=stats)+
  geom_ribbon(aes(x=days,ymin=lp,ymax=up,fill=Alpha),color="grey70",alpha=0.2)+
  geom_line(aes(x=days,y=median,color=Alpha))+
  scale_fill_manual("alpha(March 21st)",values=cols)+
  scale_colour_manual("alpha(March 21st)",values=cols)+
  theme(axis.text=element_text(size = 15, hjust = 0.5),
        axis.text.x=element_text(angle=+90,vjust=0.5, hjust=1),
        axis.title=element_text(size=18,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.position = "none",
        # legend.text=element_text(size=14),
        # legend.position=c(.1,.75),
        # legend.title=element_text(size=18,face="bold"),
        # legend.key=element_blank(),
        # legend.key.size = unit(.9, "cm"),
        # legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(x="Days", y="Comulative infected cases",title = "" )+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date( breaks = seq(as.Date("2020/02/21"),as.Date("2020/05/01"), by = "week"), date_labels = "%b-%d")+
  scale_y_continuous(limits = y_range, expand = expand_scale(mult = 0.1))

ggsave(plot = pl_aree,filename = "Plot/DifAree.pdf",
       dpi = 400, width = 15, height = 8,device = "pdf")




ggsave(plot = pl_aree + pl  + plot_layout(nrow = 1) ,filename = "Plot/DiffStrengths.pdf",
dpi = 400, width = 16, height = 8,device = "pdf")
