

optim<-c(0.0095,0.08,0.285,
         0.019,0.33,
         60,
         0.75,0.65,0.4,0.3,
         4,100,0.12)

############## Scenario 2
#### work open
model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2"
folder = "results_model_analysisScenario2"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

#source("R_func/Rt_calculation.R")
############## Scenario 2 probMask
#### work open

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2probMask2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2probMask2"
folder = "results_model_analysisScenario2probMask2"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )
#source("R_func/Rt_calculation.R")

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2probMask4.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2probMask4"
folder = "results_model_analysisScenario2probMask4"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2probMask6.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2probMask6"
folder = "results_model_analysisScenario2probMask6"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

############## Scenario 2 detection
#### work open


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection1.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection1"
folder = "results_model_analysisScenario2detection1"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )
#source("R_func/Rt_calculation.R")

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection2"
folder = "results_model_analysisScenario2detection2"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection3.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection3"
folder = "results_model_analysisScenario2detection3"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )


############## Scenario 2 detection + mask
#### work open


model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection1ProbMask2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection1ProbMask2"
folder = "results_model_analysisScenario2detection1ProbMask2"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection1ProbMask4.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection1ProbMask4"
folder = "results_model_analysisScenario2detection1ProbMask4"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection2ProbMask2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection2ProbMask2"
folder = "results_model_analysisScenario2detection2ProbMask2"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection2ProbMask4.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection2ProbMask4"
folder = "results_model_analysisScenario2detection2ProbMask4"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )



model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection2ProbMask6.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection2ProbMask6"
folder = "results_model_analysisScenario2detection2ProbMask6"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection1ProbMask6.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection1ProbMask6"
folder = "results_model_analysisScenario2detection1ProbMask6"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection3ProbMask2.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection3ProbMask2"
folder = "results_model_analysisScenario2detection3ProbMask2"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection3ProbMask4.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection3ProbMask4"
folder = "results_model_analysisScenario2detection3ProbMask4"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )

model.analysis(solver_fname =  "./net/COVID-19Piemonte.solver",
               f_time = 190,
               s_time = 1,
               solver_type = "TAUG",taueps = .001,
               n_run = 5000,
               parallel_processors = 20,
               parameters_fname = "input/plistScenario2detection3ProbMask6.csv",
               functions_fname = "R_func/Functions.R",
               ini_v = optim,
               ini_vector_mod = TRUE)

nameplot="Scenario2detection3ProbMask6"
folder = "results_model_analysisScenario2detection3ProbMask6"
system(paste('mv', 
             sprintf("COVID-19Piemonte_analysis"),
             sprintf(folder)) )