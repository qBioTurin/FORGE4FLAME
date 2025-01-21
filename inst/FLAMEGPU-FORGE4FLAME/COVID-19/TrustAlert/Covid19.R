analyze_covid_data <- function() {
  setwd("~/Desktop/COVID-19-master")
  
  model.generation(net_fname = "./net/COVID19Piemonte.PNPRO", transitions_fname = "cpp/transitions.cpp")
  
  system("mv COVID19Piemonte.* net")
  
  optim <- c(0.0095, 0.08, 0.285,
             0.019, 0.33,
             60,
             0.75, 0.65, 0.4, 0.3,
             4, 100, 0.12)
  
  model.analysis(solver_fname = "net/COVID19Piemonte.solver",
                 solver_type = "SSA",
                 f_time = 100,
                 s_time = 1,
                 ini_v = optim,
                 parameters_fname = "input/plist.csv",
                 functions_fname = "R_func/Functions.R")
  
  file_path <- "./COVID19Piemonte_analysis/COVID19Piemonte-analysis-1.trace"
  
  # Read the .trace file in CSV format
  data <- read.table(file_path, header = TRUE, sep = " ")
  
  # Calculate the number of susceptibles per day
  susceptible_columns <- c("s_a0", "s_a1", "s_a2")
  data$susceptibles <- rowSums(data[susceptible_columns])
  
  # Calculate the daily number of infected
  infected_columns <- c("i_a0_s0", "i_a0_s1", "i_a0_s2", "i_a1_s0", "i_a1_s1", "i_a1_s2", "i_a2_s0", "i_a2_s1", "i_a2_s2")
  data$infected_daily <- rowSums(data[infected_columns])
  
  # Calculate the total population per day
  total_population_columns <- c("s_a0", "s_a1", "s_a2", "e_a0", "e_a1", "e_a2", "i_a0_s0", "i_a0_s1", "i_a0_s2", "i_a1_s0", "i_a1_s1", "i_a1_s2", "i_a2_s0", "i_a2_s1", "i_a2_s2", "r_a0_s0", "r_a0_s1", "r_a0_s2", "r_a1_s0", "r_a1_s1", "r_a1_s2", "r_a2_s0", "r_a2_s1", "r_a2_s2")
  data$total_population_per_day <- rowSums(data[total_population_columns])
  
  # Calculate the percentage of infected relative to the total population
  data$percentage_infected <- (data$infected_daily / data$total_population_per_day)
  
  # Create a "day" column as an incremental index
  data$days <- seq_len(nrow(data))
  
  # Save the dataframe to a CSV file in the same folder as the input file
  data_to_save <- data[c("days", "susceptibles", "percentage_infected")]
  colnames(data_to_save) <- c("day", "susceptibles", "percentage_infected")
  output_file_path <- paste0(dirname(file_path), "/percentage_infected_by_day.csv")
  write.csv(data_to_save, file = output_file_path, row.names = FALSE)
  write.csv(data_to_save, file = "~/Desktop/cottolengo-hospital-flame-gpu-2/resources/macro_model_files/percentage_infected_by_day.csv", row.names = FALSE)
  
  # Return the resulting dataframe
  return(data_to_save)
}