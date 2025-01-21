library(readr)
PopolazionePiemonte2019 <- read_csv("./input/PopolazionePiemonte2019.csv")
Pop<-PopolazionePiemonte2019[which(PopolazionePiemonte2019$Territorio=="Piemonte"),c("Età","Value")]


a0<-paste(0:19,"anni")
a1<-paste(20:69,"anni")
a2<-c(paste(70:100,"anni"),"100 anni e più")  


pop_a0<-sum(Pop$Value[which(Pop$Età%in%a0)])
pop_a1<-sum(Pop$Value[which(Pop$Età%in%a1)])
pop_a2<-sum(Pop$Value[which(Pop$Età%in%a2)])

pop_a0
pop_a1
pop_a2

