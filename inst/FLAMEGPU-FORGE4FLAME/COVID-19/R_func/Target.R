Target<-function(output)
{
  Infect_Lq<- output[,c("c_Lq_a0","c_Lq_a1","c_Lq_a2","c_Lq_a3","c_Lq_a4")]
  Infect_lh <- output[,c("c_Lh_a0","c_Lh_a1","c_Lh_a2","c_Lh_a3","c_Lh_a4")]
  Infect<- Infect_Lq+Infect_lh
  
  Infect<- rowSums(Infect)
  
	return(Infect)
}