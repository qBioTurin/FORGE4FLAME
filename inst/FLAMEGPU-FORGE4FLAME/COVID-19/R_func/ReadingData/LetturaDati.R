library(ggplot2)
#### Fissare come working diretory la cartella COVID-19 del git

regione = "Piemonte"

setwd("./COVD19/COVID-19/dati-regioni/") 
library(readr)

listFile<-list.files("./",
                     pattern = ".csv")

listFile<-listFile[-which(listFile%in%c("dpc-covid19-ita-regioni.csv","dpc-covid19-ita-regioni-latest.csv"))]

DatiList<-lapply(listFile , function(x) {
  dati <- read_csv(x)
  
  datiReg <- dati[dati$denominazione_regione==regione,]
  
  datiReg[,c("data","nuovi_positivi", "totale_positivi", "totale_ospedalizzati", "isolamento_domiciliare",  "terapia_intensiva", "ricoverati_con_sintomi", "deceduti","totale_casi","dimessi_guariti")]
})

DatiCOVID <- do.call("rbind", DatiList)

DatiCOVID<-DatiCOVID[order(DatiCOVID$data),]

DatiCOVIDlast<-DatiCOVID[,c("data","totale_casi","totale_ospedalizzati", "deceduti")]


mean((DatiCOVID$totale_ospedalizzati)/ DatiCOVID$totale_casi)

#######################################
### Age division

time<-seq(from = as.Date("2020/02/24"),length.out = length(DatiCOVIDlast$data) , by = "day")
age_distribution <- data.frame(data=time,perc_a0 = 0,perc_a1= 0,perc_a2 = 0)

#aggiornate al 17/03,  22/03, 16/04, 26/04
index.tim<-c(0,which(age_distribution$data==time[23]),which(age_distribution$data==time[28]),which(age_distribution$data==time[53]),length(age_distribution[,1]) )

interval<-c(diff(index.tim,differences = 1))

perc_a1<-c(.01,.008,.0089,.013)
perc_a2<-c(.601,.617,.551,.5355)
perc_a3<-c(.389,.375,.4401,.4515)

lseq=lapply( 1:4, function(i){
  if(i == 1)
  {
    seq(perc_a1[i],perc_a1[i],length.out = interval[i])->perc_a0
    seq(perc_a2[i],perc_a2[i],length.out = interval[i])->perc_a1
    seq(perc_a3[i],perc_a3[i],length.out = interval[i])->perc_a2
    data.frame(perc_a0,perc_a1,perc_a2)-> seq_df
    seqMatrix<-seq_df
  }else{
    seq(perc_a1[i-1],perc_a1[i],length.out = interval[i])->perc_a0
    seq(perc_a2[i-1],perc_a2[i],length.out = interval[i])->perc_a1
    seq(perc_a3[i-1],perc_a3[i],length.out = interval[i])->perc_a2
    data.frame(perc_a0,perc_a1,perc_a2)-> seq_df
    sum_seq<-apply(seq_df,1,sum)
    seqMatrix<-seq_df/matrix(rep(sum_seq,3),ncol = 3)
  }
  
  return(seqMatrix)
})

age_distribution <- do.call("rbind",lseq) 
# 
# 
# age_distribution[1:which(age_distribution$data==time[23]),]$perc_a0<- .01
# age_distribution[1:which(age_distribution$data==time[23]),]$perc_a1<- 0.601
# age_distribution[1:which(age_distribution$data==time[23]),]$perc_a2<- 0.389
# #aggiornate al
# age_distribution[(which(age_distribution$data==time[23])+1):which(age_distribution$data==time[28]) ,]$perc_a0<- .008
# age_distribution[(which(age_distribution$data==time[23])+1):which(age_distribution$data==time[28]) ,]$perc_a1<- 0.617
# age_distribution[(which(age_distribution$data==time[23])+1):which(age_distribution$data==time[28]) ,]$perc_a2<- 0.375
# #aggiornate al 16/04
# age_distribution[(which(age_distribution$data==time[28])+1):which(age_distribution$data==time[53]) ,]$perc_a0<- .0089
# age_distribution[(which(age_distribution$data==time[28])+1):which(age_distribution$data==time[53]) ,]$perc_a1<- 0.551
# age_distribution[(which(age_distribution$data==time[28])+1):which(age_distribution$data==time[53]) ,]$perc_a2<- 0.4401
# #aggiornate al 26/04
# age_distribution[(which(age_distribution$data==time[53])+1):length(age_distribution[,1]) ,]$perc_a0<- .013
# age_distribution[(which(age_distribution$data==time[53])+1):length(age_distribution[,1]) ,]$perc_a1<- 0.5355
# age_distribution[(which(age_distribution$data==time[53])+1):length(age_distribution[,1]),]$perc_a2<- 0.4515

######################################
### perc prese dal report Seremi

Cases_a1= DatiCOVIDlast$totale_casi*age_distribution$perc_a0
Cases_a2= DatiCOVIDlast$totale_casi*age_distribution$perc_a1
Cases_a3= DatiCOVIDlast$totale_casi*age_distribution$perc_a2

DatiCOVID_ages <-data.frame(time,Cases_a1,Cases_a2,Cases_a3,DatiCOVIDlast$deceduti)

ggplot(DatiCOVID_ages,aes(x=time))+geom_line(aes(y=c(Cases_a1[1],diff(Cases_a1,differences = 1)),col="Cases_a1"))+
  geom_line(aes(y=c(Cases_a2[1],diff(Cases_a2,differences = 1)),col="Cases_a2"))+
  geom_line(aes(y=c(Cases_a3[1],diff(Cases_a3,differences = 1)),col="Cases_a3"))+
  geom_line(aes(y=c( DatiCOVIDlast$totale_casi[1],diff( DatiCOVIDlast$totale_casi,differences = 1)),col="reference"))

setwd("./COVD19/OurGit/covid-19/input/")



reference <-data.frame(c(0,0,0,Cases_a1),c(0,0,0,Cases_a2),c(0,0,0,Cases_a3),c(0,0,0,DatiCOVIDlast$deceduti))


p<-.31+.405+.12
death_percentages<-c(1-p,p)

Death_ages <-as.data.frame(matrix((DatiCOVIDlast$deceduti),ncol=1) %*% matrix(death_percentages,ncol=4))


write.table(t(reference),"./reference.csv",row.names = F, col.names= F, sep =" ")


########à province
setwd("~/Desktop/COVD19/COVID-19/dati-province/") ## update at 04/01

library(readr)

listFile<-list.files("./",
                     pattern = ".csv")

listFile<-listFile[-which(listFile%in%c("dpc-covid19-ita-province.csv","dpc-covid19-ita-province-latest.csv"))]

DatiList<-lapply(listFile , function(x) {
  dati <- read_csv(x)
  
  datiRegione<-dati[dati$denominazione_regione=="Piemonte",]
  
  a<-as.data.frame(t(datiRegione[,c("totale_casi")]) )
  a$data=unique(datiRegione$data)
  
  colnames(a) <- c(datiRegione$denominazione_provincia,"Data")
  a
})

DatiCOVIDPr <- do.call("rbind", DatiList)

DatiCOVIDPr<-DatiCOVIDPr[order(DatiCOVIDPr$Data),]

##############

write.csv(DatiCOVIDlast,"./DatiCOVID_piemonte.csv",row.names = F)
write.csv(DatiCOVID_ages,"./DatiCOVID_piemonte_ages.csv",row.names = F)
write.csv(DatiCOVIDPr,"./DatiCOVID_piemonteProvinceCasiTotali.csv",row.names = F)


