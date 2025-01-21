msqd<-function(reference, output)
{
  
  Infect_Lq<- output[-c(1:3),c("c_Lq_a0","c_Lq_a1","c_Lq_a2")]
  Infect_lh <- output[-c(1:3),c("c_Lh_a0","c_Lh_a1","c_Lh_a2")]
  Infect_detectedRSA <- output[-c(1:3),c("c_SW_a2")]
  
  Infect<- Infect_Lq+Infect_lh+Infect_detectedRSA
  
  Death <- output[-c(1:3),c("d_a1","d_a2")]
  
  infect_cases<- reference[-c(1:3),1:3]
  death_cases<- reference[-c(1:3),4]
  
  # updated at 16/04 with tree age classes
  p<-.31+.405+.12
  death_percentages<-c(1-p,p)
  
  Death_ages <- data.frame(matrix((death_cases),ncol=1) %*% matrix(death_percentages,ncol=2))
  
  infect_cases[infect_cases<1]=1
  Death_ages[Death_ages<1]=1
  
  ### Error given by RSA detection # 20/04 is number 60
  RSA_cases<-4812*.5 # scenario 1:1
  
  Error_RSA<- (Infect_detectedRSA[60-3]- RSA_cases)^2
  
  # Seremi del 26/04:
  # 20 aprile 4812 indice 60
  #### 
  
  if(length(infect_cases[,1]) != length(Infect[,1]) ){
    diff.Infect <- 10^9   ### it means that the lsoda resolution had some problems
  }else{
    #diff.Infect <- 1/length(infect_cases[,1]) * sum( rowSums( (abs( Infect - infect_cases )/infect_cases ))  +  rowSums( abs( ((Death - Death_ages)/Death_ages )) ) ) 
    diff.Infect <- 1/length(infect_cases[,1]) * sum( rowSums( (( Infect - infect_cases ) )^2)  +  rowSums( ( ((Death - Death_ages) )^2) ) ) 
  }
  
  return(diff.Infect+Error_RSA)
}

# reference <- as.data.frame(t(read.csv("input/reference.csv", header = FALSE, sep = "")))
# output=read.csv("./results_sensitivity_analysis/COVID-19Piemonte-sensitivity-1.trace", sep = "")
# output=read.csv("./results_model_caLqbration/COVID-19Piemonte-caLqbration-1.trace", sep = "")
##### RSA infetti:
# 08 aprile 4.085 indice 48
# 14 aprile 13.940 indice 54
# 20 aprile 20.642 indice 60