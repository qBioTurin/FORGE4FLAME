#Report Epidemia COVID Piemonte

#Carico librerie (alcune inutili)

library(stats)
#library(Hmisc)
library(foreign)
library(devtools)
library(outbreaks)
library(incidence)
library(reshape)
library(distcrete)
library(epitrix)
library(ggplot2)
library(EpiEstim)
library(earlyR)

#Serie Dati Incidenza Piemonte 

#piemonte
Rt_calculation<-function(dati)
{
  piemonte<-as.data.frame(dati)
  
  names(piemonte)<-"count"
  piemonte$data<- c(seq(as.Date("2020-02-21"), length = length(piemonte$count), by = 1))
  piemonte<-as.data.frame(piemonte)
  piemonte<-piemonte[piemonte$data>"2020-02-25",]
  
  
  #espando dataframe a dati individuali per processarlo con pacchetto incidence 
  
  expand_r<-function(x,n) rep(x,replace(n,n<1,1))
  piemonte_exp<-expand_r(piemonte$data, piemonte$count)
  piemonte_exp<-as.data.frame(piemonte_exp)
  
  #creo oggetto incidence da dataframe espanso
  
  piemonte_incidence_object <- incidence(piemonte_exp$piemonte_exp, interval=1L)
  
  #Fit crescita esponenziale nei primi 15 giorni per stimare R0
  
  piemonte_incidence_fit <- incidence::fit(piemonte_incidence_object[1:15])
  piemonte_incidence_fit
  
  
  x<-plot(piemonte_incidence_fit)+ylim(0,600)
  pl_exp<-x + 
    geom_point(data = piemonte,aes(y=count, x=data), col="red")+
    xlim(as.Date("2020-02-25"),as.Date("2020-03-20"))+
    geom_line(data = piemonte,aes(y=count, x=data),col="red")
  
  
  #Imposto parametri Serial Interval
  
  mu <- 4.6  # days
  sigma <- 2.9 #days
  param <- gamma_mucv2shapescale(mu, sigma/mu)
  w <- distcrete("gamma", interval = 1, shape = param$shape, scale = param$scale, w = 0)
  
  #Stimo R0
  
  growth_R0 <- lm2R0_sample(piemonte_incidence_fit$model, n=20000, w)
  
  hist_R0<-hist(growth_R0, col = "grey", border = "white", main = "Distribution of R0")
  
  quantR0<-quantile(growth_R0, probs = c(0, 0.05, 0.5, 0.95, 1))
  
  
  R0<-list(quantR0=quantR0,hist_R0=hist_R0,pl_exp=pl_exp)
  
  #Stima di Rt
  
  piemonte_incidence_object$counts
  
  #Creo finestre settimanali per avere una stima lisciata su 7 giorni
  
  T <- nrow(piemonte)
  t_start <- seq(2, length(piemonte_incidence_object$dates) - 7)
  t_end <- t_start + 7
  
  #Stima di Rt usando singola gamma distribution
  
  parametric_si <- estimate_R(piemonte_incidence_object, 
                              method = "parametric_si", config = make_config(list(mean_si = 4.6,std_si = 2.9,t_start=t_start, t_end=t_end)))
  
  
  Rt_oneGamma<-plot(parametric_si,"R")+ ggtitle("Piemonte")+theme(plot.title = element_text(hjust = 0.5))+xlim(as.Date("2020-03-01"),as.Date("2020-05-01"))+ ylim(0.5,4.5)+xlab("Data")+ylab("R")
  
  #Stima di Rt usando famiglia gamma distribution
  
  uncertain_si <- estimate_R(piemonte_incidence_object, 
                             method = "uncertain_si",
                             config = make_config(list(mean_si = 4.6,std_si = 2.9,std_mean_si=0.5,
                                                       min_mean_si = 3.1, max_mean_si = 6.1, std_std_si = 0.25,
                                                        min_std_si = 1.9, max_std_si = 3.9, n1 = 1000, n2 = 1000,
                                                       t_start=t_start, t_end=t_end)))
  
  
  Rt_familyGamma<-plot(uncertain_si,"R")+ ggtitle("Piemonte")+
    theme(plot.title = element_text(hjust = 0.5))+xlim(as.Date("2020-03-01"),max(piemonte$data))+xlab("Data")+ylab("R")+
    scale_x_date( breaks = seq(as.Date("2020/03/01"), length.out = length(unique(piemonte$data)), by = "2 week"), date_labels = "%b-%d")
  
  Rt<-list(Rt_oneGamma=Rt_oneGamma,Rt_familyGamma=Rt_familyGamma)
  return(list(R0=R0,Rt=Rt))
}



