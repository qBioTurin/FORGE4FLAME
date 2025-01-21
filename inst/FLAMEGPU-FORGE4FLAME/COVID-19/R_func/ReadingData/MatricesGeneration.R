library(readr)
library("readxl")

path="./R_func/ReadingData/contact_matrices_152_countries/"

listFile<-list.files(path = path , pattern = "1.xlsx")

L<-lapply(listFile,function(x) read_excel(paste0(path,x), sheet = "Italy") )

CompleteMall <- L[[1]] # MUestimates_all_locations
CompleteMh <- L[[2]]   # MUestimates_home
CompleteMo <- L[[3]]   # MUestimates_other_locations
CompleteMs <- L[[4]]   # MUestimates_school
CompleteMw <- L[[5]]   # MUestimates_work


########################################
popPiedmontALL <- read_csv("input/PopolazionePiemonte2019.csv")
popPiedmont <- popPiedmontALL[which(popPiedmontALL$Territorio == "Piemonte"),]
Pop<-data.frame(Età=popPiedmont$Età,Value=popPiedmont$Value)

a0<-paste(0:19,"anni")
a1<-paste(20:69,"anni")
a2<-c(paste(70:99,"anni"),"100 anni e più")  

ages<-list(1:4,5:14,15:16)
l_ages = length(ages)

####### Number of individuals for each age class considered in the contact matrix
c(0,seq(5,70,5)) -> s1
c(seq(4,74,5)) -> s2

Age_class_all<-c(paste0(s1,"-",s2), "75++" )

lapply(1:(length(Age_class_all)), function(x){
  
  if(x==(length(Age_class_all))){
    a<- c(paste(75:99,"anni"),"100 anni e più")  
  }else{
    a<-paste(s1[x]:s2[x],"anni")
  }
  sum(Pop$Value[which(Pop$Età%in%a)])
  
} ) -> l

unlist(l)->Ni
names(Ni)<-Age_class_all

############################################

row.names(CompleteMall) = row.names(CompleteMh) =  row.names(CompleteMo) = row.names(CompleteMs) = row.names(CompleteMw) = Age_class_all
colnames(CompleteMall) = colnames(CompleteMh) =  colnames(CompleteMo) = colnames(CompleteMs) = colnames(CompleteMw) = Age_class_all

Mall <-matrix(0,ncol=l_ages,nrow = l_ages)
Mh <-matrix(0,ncol=l_ages,nrow = l_ages)
Mo <-matrix(0,ncol=l_ages,nrow = l_ages)
Ms <-matrix(0,ncol=l_ages,nrow = l_ages)
Mw <-matrix(0,ncol=l_ages,nrow = l_ages)

for(i in 1:l_ages)
{
  for (j in 1: l_ages)
  {
    if(length(Ni[ages[[i]]])>1) D<-diag(Ni[ages[[i]]])
    else D<-Ni[ages[[i]]]
    
    Mh[i,j] <- sum(D %*% as.matrix(CompleteMh[ages[[i]],ages[[j]]]))/sum(Ni[ages[[i]]])  # MUestimates_home
    Mo[i,j] <- sum(D %*% as.matrix(CompleteMo[ages[[i]],ages[[j]]]))/sum(Ni[ages[[i]]])  # MUestimates_other_locations
    Ms[i,j] <- sum(D %*% as.matrix(CompleteMs[ages[[i]],ages[[j]]]))/sum(Ni[ages[[i]]])  # MUestimates_school
    Mw[i,j] <- sum(D %*% as.matrix(CompleteMw[ages[[i]],ages[[j]]]))/sum(Ni[ages[[i]]])  # MUestimates_work
    Mall[i,j] <- sum(D %*% as.matrix(CompleteMall[ages[[i]],ages[[j]]]))/sum(Ni[ages[[i]]]) 
  }
}

Mh+Mo+Ms+Mw ->Mall2

sum(Mall-Mall2)  ### Perfect, they are uquals!!


save(Mh,Mo,Ms,Mw,Mall,file="./input/ContactMatrices.RData" )


