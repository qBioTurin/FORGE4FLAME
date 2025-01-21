aic<-function(reference, output)
{
	n_samples <- length(reference)
	
	### deleting if there are points beyond te final time
	n_rep<-unique(table(output$Time))
	
	time_to_delete<-unique(output$Time)[which(table(output$Time)!=max(n_rep))]
	output<-output[-which(output$Time%in%time_to_delete),]
	n_run<-max(n_rep)
	####################
	
	
	# Get column names
	ynames <- names(output)
	
	a0=c("c_La_a0","c_Lh_a0")
	a1=c("c_La_a1","c_Lh_a1")
	a2=c("c_La_a2","c_Lh_a2")
	a3=c("c_La_a3","c_Lh_a3")
	a4=c("c_La_a4","c_Lh_a4")
	d=c("d_a2","d_a3","d_a4")
	l<-list(a0,a1,a2,a3,a4,d)
	rss<-list()
	
	for(i in length(l))
	{
		col_names <- l[[i]]
		
		# Reshape the vector to a row vector
		tmp<-as.data.frame(rowSums(output[,col_names]))
		# aggregate runs by the time
		tmp<-as.data.frame(aggregate(tmp, by=list(Time=output$Time), FUN=median))
		names(tmp)<-c("Time","Infects")
		# Create a data.frame with median and corresponding simulation time
		avg<-as.data.frame(cbind(unique(tmp$Time), tmp$Infects))
		names(avg) = c("Time","Infects")
		# Squared error
		rss[[i]]<-as.data.frame(sum((reference[,i]-avg$Infects[1:length(reference[,i])])^2))
		
	}
	rss_tot<-Reduce("+",rss)
	
	# rss<-as.data.frame(sum(na.omit((reference-avg$Infects)^2)))
	# compute AIC
	n_params <- 21
	AIC<-2*n_params+n_samples*log(rss_tot, base = exp(1))
	return(as.numeric(AIC))
}