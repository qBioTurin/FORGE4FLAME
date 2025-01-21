library(ggplot2)
library(gtable)  
library(grid)

folders <- c("./results_model_analysisScenario2" , "./results_model_analysisScenario2detection1",
             "./results_model_analysisScenario2detection2","./results_model_analysisScenario2detection3",
             "./results_model_analysisScenario2detection1ProbMask2"  ,
             "./results_model_analysisScenario2detection1ProbMask4","./results_model_analysisScenario2detection2ProbMask2" ,
             "./results_model_analysisScenario2detection2ProbMask4" ,"./results_model_analysisScenario2probMask2"  ,
             "./results_model_analysisScenario2probMask4" ,"./results_model_analysisScenario2probMask6",
             "./results_model_analysisScenario2detection1ProbMask6","./results_model_analysisScenario2detection2ProbMask6" ,
             "./results_model_analysisScenario2detection3ProbMask2","./results_model_analysisScenario2detection3ProbMask4" ,
             "./results_model_analysisScenario2detection3ProbMask6")

age_classes<-3
AgesNames=c("0-19 years old","20-69 years old","70++ years old","Comulative")

l2<-list("c_Lu_a0","c_Lq_a0","c_Lh_a0","c_Lu_a1","c_Lq_a1","c_Lh_a1","c_Lu_a2","c_Lq_a2","c_Lh_a2",
         c(paste0("c_Lu_a",0:2)), c( paste0("c_Lq_a",0:2)),c( paste0("c_Lh_a",0:2) ) ,c( paste0("c_SW_a",0:2)) )

Scenari <- data.frame(percMask=(c(0,0,0,0,.2,.4,.2,.4,.2,.4,.6,.6,.6,.2,.4,.6)),detection = (c(0,.1,.2,.3,.1,.1,.2,.2,0,0,0,.1,.2,.3,.3,.3)) )


List_dati<-lapply(1:length(folders), function(i){
  
  folder <-folders[i]
    trace=as.data.frame(read.csv( paste0("./",folder,"/COVID-19Piemonte-analysys-1.trace"), sep = ""))

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
    
    dataGG$Sympt =ordered( c(rep(rep(c("Iu","Iq","Ih"),each= length(unique(trace$Time)) ) ,(age_classes+1)),rep("Detected", length(unique(trace$Time))) ),
                             levels = c("Ih","Iq","Detected","Iu")[4:1])
    dataGG$Ages = c( rep(AgesNames,each= length(unique(trace$Time)) *3),rep("Comulative", length(unique(trace$Time))) )
    
    dataGG$PercMask = Scenari$percMask[i]
    dataGG$detection = Scenari$detection[i]
  
  return(dataGG)
})

dataGG<-do.call("rbind",List_dati)

#### Reference:
reference <- as.data.frame(t(read.csv("./input/reference.csv",
                                      header = FALSE,
                                      sep = "")))

Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(reference$V1)) 

# updated at 16/04 with tree age classes
p<-.252+.305+.24
death_percentages<-c(1-p,p)

Death_ages <-as.data.frame(matrix((reference[,age_classes+1]),ncol=1) %*% matrix(death_percentages,ncol=(age_classes-1)) )


reference<- data.frame(Time=Time[0:length(reference$V1)], reference[,1:age_classes],rowSums(reference[,1:age_classes]),Death_ages,reference[,age_classes+1])

reference <- reference[which(reference$Time >= "2020-02-24" ),]

reference_big_hist<- lapply(1:(age_classes+1), function(j){
  data.frame(Time=reference$Time,I=reference[,j+1],Idiff=c(reference[1,j+1], diff(reference[,j+1],lag = 1) ),Ages=AgesNames[j])
})
reference_big_hist<-do.call("rbind",reference_big_hist) 


pl<-ggplot(data=dataGG[which(dataGG$Ages == "Comulative"),],
           aes(x=Time,y=Idiff) )+
  geom_bar( aes(fill=Sympt,col=Sympt ),
            stat="identity",alpha=.5)+
  geom_line(data=reference_big_hist[which(reference_big_hist$Ages == "Comulative"),],
            aes(x=Time,y=Idiff,col="red"),size=1.1)+
  scale_colour_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"="red", "Detected"="purple"),
                      limits = c("Ih","Iq","Iu", "Detected","red"),
                      labels = c("Ih","Iq","Iu", "Detected","Surveillance") )+
  scale_fill_manual("" , values= c("Ih"="#4E79A7","Iq"="#A0CBE8","Iu"="#F28E2B", "red"=NA,"Detected"="purple"),
                    limits = c("Ih","Iq","Iu","Detected","red"),
                    labels = c("Ih","Iq","Iu","Detected","Surveillance") )+
  facet_grid(detection ~ PercMask,scales = "free")+
  theme(axis.text=element_text(size = 25, hjust = 0.5),
        axis.text.x=element_text(angle=+90),
        axis.title=element_text(size=28,face="bold"),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=30, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=24),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(5,5,5,5),"mm"),
        strip.text = element_text(size = 20))+
  labs(x="Days", y="New Daily Infected cases" )+
  geom_vline(xintercept=as.Date("2020-02-25"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-02-26"), label="\n First public restriction", y= Inf, colour="#47606c",size=6,hjust = 0) +
  geom_vline(xintercept=as.Date("2020-03-08"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-09"), label="\n\n\n Second public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-03-21"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-03-22"), label="\n\n\n\n\n Third public restriction", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-05-04"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-05-05"), label="\n First release", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-05-18"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-05-19"), label="\n\n\n Second release", y= Inf, colour="#47606c", size=6,hjust = 0)+
  geom_vline(xintercept=as.Date("2020-06-12"), colour="#47606c", linetype = "longdash" )+
  annotate("text",x=as.Date("2020-06-13"), label="\n\n\n\n\n Third release", y= Inf, colour="#47606c", size=6,hjust = 0)+
  scale_x_date( breaks = seq(as.Date("2020/02/21"), length.out = length(unique(dataGG$Time)), by = "2 week"), date_labels = "%b-%d")+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))



z <- ggplotGrob(pl)

#  New strip at the top
z <- gtable_add_rows(z, z$height[7], pos = 6)  # New row added below row 6
#  New strip to the right
z <- gtable_add_cols(z, z$widths[12], pos = 12)  # New column added to the right of column 12

# Check the layout
# gtable_show_layout(z)   
# New strip spans columns 5 to 11

z <- gtable_add_grob(z, 
                     list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                          textGrob("Decreasing of the contact (mask, etc)", gp = gpar(cex = 1.1, fontface = 'bold', col = "black"))), 
                     t=7, l=5, b=7, r=11, name = c("a", "b"))

z <- gtable_add_grob(z, 
                     list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                          textGrob("Detection rate from 2020-05-04", rot = -90,  
                                   gp = gpar(cex = 1.1, fontface = 'bold', col = "black"))), 
                     t=9, l=13, b=15, r=13, name = c("a", "b"))

# Add small gap between strips - below row 6
z <- gtable_add_rows(z, unit(2/10, "line"), 7)
z <- gtable_add_cols(z, unit(2/10, "line"), 12)

# Draw it
grid.newpage()
grid.draw(z)

ggsave(plot = pl,filename = "ScaleFree.pdf",
       dpi = 400, width = 40, height = 16,device = "pdf")

##### Rt

