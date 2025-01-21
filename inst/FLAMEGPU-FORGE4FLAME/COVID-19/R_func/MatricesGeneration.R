
library("readxl")

path="./contact_matrices_152_countries/"

listFile<-list.files(path = path , pattern = "1.xlsx")

L<-lapply(listFile,function(x) read_excel(paste0(path,x), sheet = "Italy") )

CompleteMall <- L[[1]] # MUestimates_all_locations
CompleteMh <- L[[2]]   # MUestimates_home
CompleteMo <- L[[3]]   # MUestimates_other_locations
CompleteMs <- L[[4]]   # MUestimates_school
CompleteMw <- L[[5]]   # MUestimates_work

CompleteMh+CompleteMo+CompleteMs+CompleteMw ->CompleteMall2

sum(CompleteMall-CompleteMall2)  ### Perfect, they are uquals!!


Age_class <-c(paste0(seq(0,70,5),"-",seq(4,74,5)), "75++" )

row.names(CompleteMall) = row.names(CompleteMh) =  row.names(CompleteMo) = row.names(CompleteMs) = row.names(CompleteMw) = Age_class
colnames(CompleteMall) = colnames(CompleteMh) =  colnames(CompleteMo) = colnames(CompleteMs) = colnames(CompleteMw) = Age_class


#
# c("0-19 years old","20-39 years old","40-59 years old","60-79 years old","80++ years old","Comulative")
# 
# a0<-1:4
# a1<-5:8
# a2<-9:12
# a3<-13:15
# a4<-16
# ages<-list(a0,a1,a2,a3,a4)


# c("0-19 years old","20-69 years old","70++ years old","Comulative")

a0<-1:4
a1<-5:14
a2<-15:16
ages<-list(a0,a1,a2)

l_ages<-length(ages)

Mh <- matrix(0,ncol=l_ages,nrow = l_ages)  # MUestimates_home
Mo <- matrix(0,ncol=l_ages,nrow = l_ages)  # MUestimates_other_locations
Ms <- matrix(0,ncol=l_ages,nrow = l_ages)  # MUestimates_school
Mw <- matrix(0,ncol=l_ages,nrow = l_ages)  # MUestimates_work
Mall<-matrix(0,ncol=l_ages,nrow = l_ages)

for(i in 1:l_ages)
{
  for (j in 1: l_ages)
  {
    Mh[i,j] <- sum(CompleteMh[ages[[i]],ages[[j]]])  # MUestimates_home
    Mo[i,j] <- sum(CompleteMo[ages[[i]],ages[[j]]])  # MUestimates_other_locations
    Ms[i,j] <- sum(CompleteMs[ages[[i]],ages[[j]]])  # MUestimates_school
    Mw[i,j] <- sum(CompleteMw[ages[[i]],ages[[j]]])  # MUestimates_work
    Mall[i,j] <- sum(CompleteMall[ages[[i]],ages[[j]]])
    
  }
}

Mh+Mo+Ms+Mw ->Mall2

sum(Mall-Mall2)  ### Perfect, they are uquals!!

save(Mh,Mo,Ms,Mw,Mall,file="./input/ContactMatrices.RData" )

