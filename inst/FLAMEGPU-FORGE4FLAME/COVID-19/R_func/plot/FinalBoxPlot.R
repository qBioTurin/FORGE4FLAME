setwd("~/Desktop/COVD19/OurGit/covid-19/")

id=1
library(ggplot2)
library(cowplot)

trace50=as.data.frame(read.csv( "./../../Results/Model1/Model1FinalBoxplot/COVID-19Piemonte-analysys-1.trace", sep = ""))
trace60=as.data.frame(read.csv( "./../../Results/Model2/Model2FinalBoxPlot/COVID-19Piemonte-analysys-1.trace", sep = ""))
trace90=as.data.frame(read.csv( "./../../Results/Model3/Model3FinalBoxPlot/COVID-19Piemonte-analysys-1.trace", sep = ""))

trace <- rbind( trace50,trace60,trace90)

trace$Perc <- c(rep(".5",length(trace50[,1]) ) ,rep(".6",length(trace60[,1]) ),rep(".9",length(trace90[,1])) )

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


death_percentages<-c(0.007,0.046,.375,.572)

Death_ages <-as.data.frame(matrix((reference[,6]),ncol=1) %*% matrix(death_percentages,ncol=4))


reference<- data.frame(Time=trace$Time[0:length(reference$V1)], reference[,1:5],rowSums(reference[,1:5]),Death_ages,reference[,6])
hyst <- reference

Time <- seq(as.Date("2020/02/21"), by = "day", length.out = length(unique(trace$Time))) 

y_names <-names(trace)
# Get the indexes of the places to consider


a0=c("c_Li_a0","c_Lh_a0")
a1=c("c_Li_a1","c_Lh_a1")
a2=c("c_Li_a2","c_Lh_a2")
a3=c("c_Li_a3","c_Lh_a3")
a4=c("c_Li_a4","c_Lh_a4")
d=c("d_a1","d_a2","d_a3","d_a4")
l<-list(a0,a1,a2,a3,a4,c(a0,a1,a2,a3,a4),"d_a1","d_a2","d_a3","d_a4",d)

titleList<-c("Infects 00-19 years old","Infects 20-39 years old","Infects 40-59 years old","Infects 60-79 years old","Infects 80++ years old","Infects all ages",
             "Deaths 20-39 years old","Deaths 40-59 years old","Deaths 60-79 years old","Deaths 80++ years old","Deaths all ages")
ylabel<-c("Total cases in a0","Total cases in a1","Total cases in a2","Total cases in a3","Total cases in a4","Total cases",
          "Total deaths in a1","Total deaths in a2","Total deaths in a3","Total deaths in a4","Total deaths")


AgesNames=c("0-19 years old","20-39 years old","40-59 years old","60-79 years old","80++ years old","Comulative")

reference_big_hist<- lapply(1:6, function(j){
  data.frame(Time=reference$Time,I=reference[,j+1],Ages=AgesNames[j])
})
reference_big_hist<-do.call("rbind",reference_big_hist)  

Trace_big_hist<-data.frame( Time = rep(trace$Time, 6 ),
                            I = c(rowSums(trace[,  a0 ] ), rowSums(trace[,  a1 ] ), rowSums(trace[,  a2 ] ),rowSums(trace[,  a3 ] ),rowSums(trace[,  a4 ] ), rowSums(trace[,  l[[6]] ] ) ),
                            Perc = rep(trace$Perc,6),
                            Restr = rep(trace$Restr,6),
                            Ages=rep(AgesNames,each=length(trace$Time)) )

unique(Trace_big_hist$Time)->weekly

weekly[seq(1,length(weekly),7)]->weekly

Violinplot<-    ggplot(data=Trace_big_hist[which(Trace_big_hist$Ages == "Comulative" & Trace_big_hist$Time %in% weekly),],
       aes(x=factor(Time),y=I,fill=Perc,col=Perc) )+
  geom_boxplot(alpha=.5)+
  scale_fill_manual("Undetected cases:",
                    values=c(".5"="darkorange",".6"="darkgreen",".9"="darkblue"),
                    labels =c("1:1 on average","1:1.5 on average","1:10 on average"))+
    scale_colour_manual("Undetected cases:",
                      values=c(".5"="darkorange",".6"="darkgreen",".9"="darkblue"),
                      labels = c("1:1 on average","1:1.5 on average","1:10 on average"))+
  theme(axis.text=element_text(size = 28, hjust = 0.5),
        axis.title=element_text(size=30,face="bold"),
        axis.text.x = element_text(angle=+90),
        axis.line = element_line(colour="black"),
        plot.title=element_text(size=20, face="bold", vjust=1, lineheight=0.6),
        legend.text=element_text(size=28),
        legend.position=c(.1,.85),
        legend.title=element_text(size=30,face="bold"),
        legend.key=element_blank(),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        plot.margin=unit(c(10,5,5,5),"mm"), 
        strip.background = element_blank(),
        strip.text.x = element_blank() )+
  labs(y="Comulative infected cases", x="Days",title = "" )+
scale_x_discrete( labels = c("Feb-21", "Feb-28" ,"Mar-06", paste0("Mar-", c(13,20,27) ), "Apr-03", paste0("Apr-", c(10,17,24) ), "May-01") )+
  scale_y_continuous(breaks= scales::pretty_breaks(n = 8))

ggsave(plot = Violinplot,
         filename = "../../PaperPlots/ViolinPlot.pdf",
         dpi = 400, width = 25, height = 15,device = "pdf")






