
##### First Step: upload the library EPIMOD and download the Docker containers
# To install the library: https://github.com/qBioTurin/epimod

library(epimod)
#downloadContainers()

#### Second Step: generate the solver of the Petri Net representing the COVID19 model
# saved in COVID-19Piemonte.PNPRO

model.generation(net_fname = "net/COVID-19Piemonte.PNPRO", 
                 transitions_fname = "cpp/transitions.cpp")

#### Third Step: run the calibration phase in order to estimate an optimal parameter configuration

# three parameters represent the probability of infection for each age class,
# two parameters represent the  death rate for the hospitalized patients,
# four parameters reflect the governmental action strength for Work and Other
#      at time epoch: March 8th, March 21st,
# one parameter describes the intensity of the population response (i.e., $k$),
# two parameters are the initial condition for the undetected and quarantine infected individuals,
# the remainder parameter represents the detection rate for the third age class starting from
#     the $1^{st}$ April.

##The calibration is optional, we already have the best parameters

#optimStart<-c( 0.0085, 0.086,0.285, 
#              0.024,0.28,
#               50,
#               0.7,0.6,0.4,0.2, 
#               4,90,0.1)

#Lb<-c( 0.005, 0.05,0.1,
#       0.01,0.1,
#       10,
#       0.7,0.6,0.4,0.2,
#       3,20,0)

#Ub<-c( 0.01, 0.1,0.5,
#       0.1,0.5,
#       100,
#       0.7,0.6,0.4,0.2,
#       5,150,0.2)


#model.calibration(solver_fname = "net/COVID-19Piemonte.solver",
#                  f_time = 71,
#                  s_time = 1,
#                  parameters_fname = "input/plist.csv",
#                  functions_fname = "R_func/Functions.R",
#                  distance_measure_fname = "R_func/msqd.R",
#                  reference_data = "input/reference.csv",
#                  timeout = "1d",
#                  ini_v = optimStart,
#                  lb= Lb,
#                  ub= Ub )

#####################################################################
##### Here we exploit te best configuration discovered to solve the 
##### system of ODEs corresponding to the model.

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 71,
               s_time = 1,
               n_config = 1,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

# Automatically the plots regarding the Infected individuals and deaths
# are generated

source('./R_func/plot/PlotModelAnalysis.R')

###### Here to generate the simulations to obtain the results showed in Fig 3,
# for the COVID-19 spread and the different government control interventions

# the script to generate the plots are folder name dependent, for this reason
# every time the simulations are ended, the folder name (results_model_analysis)
# is changed

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 10000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

folder = "results_model_analysisStochEstimated"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )


optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.75,0.65,
         4,100,0.12)


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 10000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

folder = "results_model_analysisStochWorst"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )


#### best option
optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.1,0.05,
         4,100,0.12)

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .01,
               n_run = 10000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

folder = "results_model_analysisStochBest"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

source("R_func/plot/HistogramDistributionDifferentAlpha.R")

#########################
#### Stochastic simulation

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 70,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plist.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario1"
folder = "results_model_analysisScenario1"
system(paste('mv', 
            sprintf("COVID-19Piemonte_analysis"),
            sprintf(folder)) )
      
source("R_func/plot/StochComulativeInfects.R")
source("R_func/plot/Plot_StochDiff.R")

