library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)

wm <- model

agents_df <- do.call(rbind, lapply(names(wm$agents), function(nm) {
  data.frame(entry_type = wm$agents[[nm]]$entry_type,
             type = nm,
             stringsAsFactors = FALSE)
}))

agents_df %>%
  group_by(entry_type) %>%
  summarise(types = list(unique(type)),
            n = n())



# agentsPS_allfiles <- as.list(paste0(list.dirs(".", recursive = FALSE), "/AGENT_POSITION_AND_STATUS.csv"))
# agentPS <- lapply(agentsPS_allfiles, read_csv)
# agentPS <- lapply(agentPS, "colnames<-",  c( "time", "id", "agent_type", "x", "y", "z",
#                                              "disease_state"))



#agent_with_time_window <- Filter(function(x) x$entry_type == "Time window", canvasObjects$agents)
#agent_with_daily_rate<- Filter(function(x) x$entry_type == "Daily Rate", canvasObjects$agents)
agent_with_time_window <- agents_df %>% filter(agents_df$entry_type == "Time window")
agent_with_daily_rate<- agents_df %>% filter(agents_df$entry_type == "Daily Rate")
agents <- c(agent_with_time_window$type, agent_with_daily_rate$type)
agents
disease_name = c("Susceptible", "Exposed", "Infected", "Recovered", "Died")
disease_name

fixed_colors <- c("Susceptible" = "green", "Exposed" = "blue", "Infected" = "red", "Recovered" = "purple", "Died" = "black")

seeds_no_counter <- list.dirs("Hospital_NoCountermeasures", recursive = FALSE)
seeds_surgical40 <- list.dirs("Hospital_Surgical40", recursive = FALSE)
seeds_surgical90 <- list.dirs("Hospital_Surgical90", recursive = FALSE)
seeds_ffp240 <- list.dirs("Hospital_FFP240", recursive = FALSE)
seeds_ffp290 <- list.dirs("Hospital_FFP290", recursive = FALSE)
seeds_quarantineswab <- list.dirs("Hospital_QuarantineSwab", recursive = FALSE)
seeds_quarantineswabffp240 <- list.dirs("Hospital_QuarantineSwab_FFP240", recursive = FALSE)
seeds_quarantineswabffp290 <- list.dirs("Hospital_QuarantineSwab_FFP290", recursive = FALSE)
seeds_quarantineswabsur40 <- list.dirs("Hospital_QuarantineSwab_Surgical40", recursive = FALSE)
seeds_quarantineswabsur90 <- list.dirs("Hospital_QuarantineSwab_Surgical90", recursive = FALSE)

read_runs_from_seeds <- function(seeds) {

  run_list <- lapply(seeds, function(seed) {
    # Percorsi dei file nella cartella
    agent_file <- file.path(seed, "AGENT_POSITION_AND_STATUS.csv")
    evo_file   <- file.path(seed, "evolution.csv")
    counter_file <- file.path(seed, "counters.csv")

    # Controlla se i file esistono
    if (!file.exists(agent_file) || !file.exists(evo_file) || !file.exists(counter_file)) {
      warning(paste("File mancanti per seed:", seed))
      return(NULL)
    }

    # Leggi i file
    df_agent <- read.csv(agent_file)
    colnames(df_agent) <- c("time", "id", "agent_type", "x", "y", "z", "disease_state")

    df_evo <- read.csv(evo_file)

    df_count <- read.csv(counter_file, skip = 2)
    colnames(df_count) <- c("day","COUNTERS_CREATED_AGENTS_WITH_RATE","COUNTERS_KILLED_AGENTS_WITH_RATE","AGENTS_IN_QUARANTINE","SWABS","NUM_INFECTED_OUTSIDE")
    zero_row <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(df_count)))
    colnames(zero_row) <- colnames(df_count)
    df_new <- rbind(zero_row, df_count)


    # Ritorna una lista con i tre dataframe + seed
    list(agentPS = df_agent, evolution = df_evo, counters = df_new, seed = seed)
  })

  return(run_list)
}


all_runs_nomask <- read_runs_from_seeds(seeds_no_counter)
all_runs_surgical40 <- read_runs_from_seeds(seeds_surgical40)
all_runs_surgical90 <- read_runs_from_seeds(seeds_surgical90)
all_runs_ffp240 <- read_runs_from_seeds(seeds_ffp240)
all_runs_ffp290 <- read_runs_from_seeds(seeds_ffp290)
all_runs_quarantineswab <- read_runs_from_seeds(seeds_quarantineswab)
all_runs_quarantineswab[[42]]$agentPS[327013, "disease_state"] <- "3"
all_runs_quarantineswabsur40 <- read_runs_from_seeds(seeds_quarantineswabsur40)
all_runs_quarantineswabsur90 <- read_runs_from_seeds(seeds_quarantineswabsur90)
all_runs_quarantineswabffp240 <- read_runs_from_seeds(seeds_quarantineswabffp240)
all_runs_quarantineswabffp290 <- read_runs_from_seeds(seeds_quarantineswabffp290)

#saveRDS(all_runs_nomask)

processedAPS_from_runs <- function(all_runs, agents, disease_name, fixed_colors) {

  # estrai agentPS (se nel run la lista è chiamata differentemente, adattare qui)
  agentPS_list <- lapply(all_runs, function(run) {
    # se run è NULL o non contiene agentPS, restituisci NULL per mantenere la posizione
    if (is.null(run) || is.null(run$agentPS)) return(NULL)
    run$agentPS
  })

  # funzione di processing per un singolo simulation_log
  process_single <- function(simulation_log) {
    if (is.null(simulation_log)) return(NULL)

    sim <- simulation_log %>%
      # 1) day (assumo time in minuti come nel tuo esempio)
      mutate(day = ceiling(time / 1440)) %>%
      # 2) mappa agent_type e disease_state usando i vettori passati
      mutate(agent_type = agents[as.numeric(agent_type) + 1],
             disease_state = disease_name[as.numeric(disease_state) + 1])

    # se non ci sono day validi
    if (all(is.na(sim$day))) return(tibble::tibble())

    global_min_day <- min(sim$day, na.rm = TRUE)
    global_max_day <- max(sim$day, na.rm = TRUE)
    all_days <- seq(global_min_day, global_max_day)

    simulation_log_full <- sim %>%
      arrange(id, day, time) %>%
      group_by(id, agent_type) %>%
      tidyr::complete(day = seq(global_min_day, max(day, na.rm = TRUE))) %>%
      arrange(id, day, time) %>%
      tidyr::fill(disease_state, .direction = "down") %>%  # propagazione ultimo stato
      ungroup()

    # aggregazione giornaliera per coppie agent_type / disease_state
    daily_summary <- simulation_log_full %>%
      filter(!is.na(disease_state)) %>%
      group_by(agent_type, disease_state, day) %>%
      summarise(count = n_distinct(id), .groups = "drop")

    # completa su agent_type, disease_state e day; riempi con 0
    daily_complete <- daily_summary %>%
      ungroup() %>%
      mutate(disease_state = factor(disease_state, levels = names(fixed_colors))) %>%
      tidyr::complete(agent_type, disease_state = disease_state, day = all_days, fill = list(count = 0)) %>%
      arrange(agent_type, disease_state, day) %>%
      mutate(count = as.integer(count))

    # rimuovi combinazioni agent_type/disease_state che non sono mai apparse
    seen_pairs <- daily_summary %>% distinct(agent_type, disease_state)
    daily_complete <- daily_complete %>% semi_join(seen_pairs, by = c("agent_type", "disease_state"))

    return(daily_complete)
  }

  # applica a tutta la lista e restituisci la lista di dataframes/tibble
  result_list <- lapply(agentPS_list, process_single)
  return(result_list)
}

agentPS_nocont <-  processedAPS_from_runs(
  all_runs = all_runs_nomask,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_surgical40 <-processedAPS_from_runs(
  all_runs = all_runs_surgical40,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_surgical90 <- processedAPS_from_runs(
  all_runs = all_runs_surgical90,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_ffp240 <- processedAPS_from_runs(
  all_runs = all_runs_ffp240,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_ffp290 <-  processedAPS_from_runs(
  all_runs = all_runs_ffp290,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_quarantineswab <- processedAPS_from_runs(
  all_runs = all_runs_quarantineswab,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)

agentPS_quarantineswabsur40 <- processedAPS_from_runs(
  all_runs = all_runs_quarantineswabsur40,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_quarantineswabsur90 <- processedAPS_from_runs(
  all_runs = all_runs_quarantineswabsur90,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_quarantineswabffp240 <- processedAPS_from_runs(
  all_runs = all_runs_quarantineswabffp240,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)
agentPS_quarantineswabffp290 <- processedAPS_from_runs(
  all_runs = all_runs_quarantineswabffp290,
  agents = agents,
  disease_name = disease_name,
  fixed_colors = fixed_colors
)

update_agentPS <- function(all_runs, processedAPS) {
  if (length(all_runs) != length(processedAPS)) {
    warning("ATTENZIONE: processedAPS e all_runs hanno lunghezze diverse!")
  }

  # Aggiorna agentPS run-by-run
  for (i in seq_along(all_runs)) {
    if (is.null(all_runs[[i]]$agentPS)) {
      warning(paste("Run", i, "ha agentPS NULL. Skip."))
      next
    }
    if (i > length(processedAPS) || is.null(processedAPS[[i]])) {
      warning(paste("Run", i, "non ha processedAPS corrispondente. Skip."))
      next
    }
    # Assegna il dataframe aggiornato
    all_runs[[i]]$agentPS <- processedAPS[[i]]
  }

  return(all_runs)
}

all_runs_nocont <- update_agentPS(all_runs_nomask, agentPS_nocont)
all_runs_surgical40 <- update_agentPS(all_runs_surgical40, agentPS_surgical40)
all_runs_surgical90 <- update_agentPS(all_runs_surgical90, agentPS_surgical90)
all_runs_ffp240 <- update_agentPS(all_runs_ffp240, agentPS_ffp240)
all_runs_ffp290 <- update_agentPS(all_runs_ffp290, agentPS_ffp290)
all_runs_quarantineswab <- update_agentPS(all_runs_quarantineswab, agentPS_quarantineswab)
all_runs_quarantineswabsur40 <- update_agentPS(all_runs_quarantineswabsur40, agentPS_quarantineswabsur40)
all_runs_quarantineswabsur90 <- update_agentPS(all_runs_quarantineswabsur90, agentPS_quarantineswabsur90)
all_runs_quarantineswabffp240 <- update_agentPS(all_runs_quarantineswabffp240, agentPS_quarantineswabffp240)
all_runs_quarantineswabffp290 <- update_agentPS(all_runs_quarantineswabffp290, agentPS_quarantineswabffp290)


calc_qaly_cost <- function(agentPS,
                           evolution_df = NULL,   # per mascherine, tutti gli agenti
                           agg_quarantine = NULL, # opzionale, agenti in quarantena
                           mask_flag = FALSE,
                           mask_coverage = 0.4,
                           mask_start_day = 18,
                           cost_per_infected = 50,   # €/paziente/giorno
                           cost_per_mask = 0.5,
                           u_infected = 0.65,
                           days_in_model = 105,
                           quarantine_flag = FALSE,
                           quarantine_cost_per_day = 100,
                           testing_flag = FALSE,
                           cost_per_test = 20) {

  # --- 1. QALY giornaliera per paziente dai run ---
  per_run_list <- lapply(agentPS, function(df) {
    if(is.null(df) || nrow(df) == 0) return(tibble())
    df %>%
      filter(agent_type == "Patient") %>%
      group_by(day) %>%
      summarise(
        total_patients = sum(count, na.rm = TRUE),
        infected = sum(count[disease_state == "Infected"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(p_infected = ifelse(total_patients > 0, infected / total_patients, 0))
  })

  # unisco i run
  per_run_df <- imap_dfr(per_run_list, ~ mutate(.x, run = .y)) %>%
    filter(!is.na(day))

  # calcolo media giornaliera per paziente
  df_mean <- per_run_df %>%
    group_by(day) %>%
    summarise(
      mean_total_patients = mean(total_patients, na.rm = TRUE),
      mean_p_infected = mean(p_infected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(day) %>%
    mutate(
      mean_noninfected = 1 - mean_p_infected,                    # percentuale non infetti
      qaly_day = (mean_noninfected * 1 + mean_p_infected * u_infected), # media per paziente
      mean_infected = mean_total_patients * mean_p_infected      # numero medio infetti giornaliero
    )

  # --- 2. Mascherine ---
  if(mask_flag & !is.null(evolution_df)) {
    df_masks <- evolution_df %>%
      mutate(total_agents = rowSums(select(., -Day), na.rm = TRUE),
             masks_today = ifelse(Day >= mask_start_day, total_agents * mask_coverage, 0),
             masks_cost = masks_today * cost_per_mask) %>%
      select(day = Day, masks_cost)
  } else {
    df_masks <- tibble(day = df_mean$day, masks_cost = 0)
  }

  # --- 3. Quarantena ---
  if(quarantine_flag & !is.null(agg_quarantine)) {
    df_quarantine <- agg_quarantine %>%
      rename(day = day, quarantine_today = mean_quarantine) %>%
      mutate(quarantine_cost = quarantine_today * quarantine_cost_per_day) %>%
      select(day, quarantine_cost)
  } else {
    df_quarantine <- tibble(day = df_mean$day, quarantine_cost = 0)
  }

  # --- 4. Tamponi giornalieri sui pazienti ---
  if(testing_flag) {
    df_testing <- df_mean %>%
      mutate(tests_cost = mean_total_patients * cost_per_test) %>%
      select(day, tests_cost)
  } else {
    df_testing <- tibble(day = df_mean$day, tests_cost = 0)
  }

  # --- 5. Costi infezioni giornalieri ---
  df_cost <- df_mean %>%
    left_join(df_masks, by = "day") %>%
    left_join(df_quarantine, by = "day") %>%
    left_join(df_testing, by = "day") %>%
    mutate(
      cost_infections = mean_infected * cost_per_infected,
      total_cost = masks_cost + cost_infections + quarantine_cost + tests_cost
    )

  # --- 6. Output finale ---
  tibble(
    qaly_total = sum(df_cost$qaly_day, na.rm = TRUE),  # somma QALY media giornaliera
    total_cost = sum(df_cost$total_cost, na.rm = TRUE),
    est_mean_infected = sum(df_cost$mean_infected, na.rm = TRUE),
    masks_cost = sum(df_cost$masks_cost, na.rm = TRUE),
    quarantine_cost = sum(df_cost$quarantine_cost, na.rm = TRUE),
    testing_cost = sum(df_cost$tests_cost, na.rm = TRUE),
    infection_cost = sum(df_cost$cost_infections, na.rm = TRUE)
  )
}

#Scenario no countermeasures
counters_nocont <- lapply(all_runs_nomask, function(run) run$counters)
evolution_list_nocont <- lapply(all_runs_nomask, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution <- bind_rows(evolution_list_nocont, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")

agg_mean_quarantine_nocont <- counters_nocont %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE, na.rm = TRUE), .groups = "drop")

stats_nocont <- calc_qaly_cost(
  agentPS = agentPS_nocont,
  agg_quarantine = agg_mean_quarantine_nocont,
  evolution_df = agg_mean_evolution,
  mask_flag = FALSE,
  quarantine_flag = FALSE
)

# Scenario mascherina chirurgica 40%
counters_surgical40 <- lapply(all_runs_surgical40, function(run) run$counters)
evolution_list_surgical40 <- lapply(all_runs_surgical40, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_surgical40 <- bind_rows(evolution_list_surgical40, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_surgical40 <- counters_surgical40 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE, na.rm = TRUE), .groups = "drop")

stats_surgical40 <- calc_qaly_cost(
  agentPS = agentPS_surgical40,
  evolution_df = agg_mean_evolution_surgical40,
  agg_quarantine = agg_mean_quarantine_surgical40,
  mask_flag = TRUE,
  mask_coverage = 0.4,
  cost_per_mask = 0.5,
  quarantine_flag = FALSE
)

###Scenario mascherina chirurgica 90%
counters_surgical90 <- lapply(all_runs_surgical90, function(run) run$counters)
evolution_list_surgical90 <- lapply(all_runs_surgical90, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_surgical90 <- bind_rows(evolution_list_surgical90, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_surgical90 <- counters_surgical90 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_surgical90 <- calc_qaly_cost(
  agentPS = agentPS_surgical90,
  evolution_df = agg_mean_evolution_surgical90,
  agg_quarantine = agg_mean_quarantine_surgical90,
  mask_flag = TRUE,
  mask_coverage = 0.9,
  quarantine_flag = FALSE,
  cost_per_mask = 0.5
)

###Scenario mascherina FFP2 40%
counters_FFP240 <- lapply(all_runs_ffp240, function(run) run$counters)
evolution_list_FFP249 <- lapply(all_runs_ffp240, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_ffp240 <- bind_rows(evolution_list_surgical90, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_FFP240 <- counters_FFP240 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_FFP240 <- calc_qaly_cost(
  agentPS = agentPS_ffp240,
  evolution_df = agg_mean_evolution_ffp240,
  agg_quarantine = agg_mean_quarantine_FFP240,
  mask_flag = TRUE,
  mask_coverage = 0.9,
  cost_per_mask = 1.2,
  quarantine_flag = FALSE
)

###Scenario mascherina FFP2 90%
counters_FFP290 <- lapply(all_runs_ffp290, function(run) run$counters)
evolution_list_ffp290 <- lapply(all_runs_ffp290, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_ffp290 <- bind_rows(evolution_list_ffp290, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_FFP290 <- counters_FFP290 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_FFP290 <- calc_qaly_cost(
  agentPS = agentPS_ffp290,
  evolution_df = agg_mean_evolution_ffp290,
  agg_quarantine = agg_mean_quarantine_FFP290,
  mask_flag = TRUE,
  mask_coverage = 0.9,
  cost_per_mask = 1.2,
  quarantine_flag = FALSE
)

###Scenario quarantena
counters_quarantine <- lapply(all_runs_quarantineswab, function(run) run$counters)
evolution_list_quarantine <- lapply(all_runs_quarantineswab, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_quarantine <- bind_rows(evolution_list_quarantine, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_quarantine <- counters_quarantine %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_quarantine <- calc_qaly_cost(
  agentPS = agentPS_quarantineswab,
  evolution_df = agg_mean_evolution_quarantine,
  agg_quarantine = agg_mean_quarantine_quarantine,
  mask_flag = FALSE,
  quarantine_flag = TRUE,
  testing_flag = TRUE
  )

###Scenario quarantena surgical 40%
counters_quarantine_surgical40 <- lapply(all_runs_quarantineswabsur40, function(run) run$counters)
evolution_list_quarantine_surgical40 <- lapply(all_runs_quarantineswabsur40, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_quarantine_surgical40 <- bind_rows(evolution_list_quarantine_surgical40, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_quarantinesur40 <- counters_quarantine_surgical40 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_quarantine_sur40 <- calc_qaly_cost(
  agentPS = agentPS_quarantineswabsur40,
  evolution_df = agg_mean_evolution_quarantine_surgical40,
  agg_quarantine = agg_mean_quarantine_quarantinesur40,
  mask_flag = TRUE,
  quarantine_flag = TRUE,
  testing_flag = TRUE,
  mask_coverage = 0.4,
  cost_per_mask = 0.5,
)

###Scenario quarantena surgical 90%
counters_quarantine_surgical90 <- lapply(all_runs_quarantineswabsur90, function(run) run$counters)
evolution_list_quarantine_surgical90 <- lapply(all_runs_quarantineswabsur90, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_quarantine_surgical90 <- bind_rows(evolution_list_quarantine_surgical90, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_quarantinesur90 <- counters_quarantine_surgical90 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_quarantine_sur90 <- calc_qaly_cost(
  agentPS = agentPS_quarantineswabsur90,
  evolution_df = agg_mean_evolution_quarantine_surgical90,
  agg_quarantine = agg_mean_quarantine_quarantinesur90,
  mask_flag = TRUE,
  quarantine_flag = TRUE,
  testing_flag = TRUE,
  mask_coverage = 0.4,
  cost_per_mask = 0.5,
)

###Scenario quarantena FFP2 40%
counters_quarantine_FFP240 <- lapply(all_runs_quarantineswabffp240, function(run) run$counters)
evolution_list_quarantine_FFP240 <- lapply(all_runs_quarantineswabffp240, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_quarantine_ffp240 <- bind_rows(evolution_list_quarantine_FFP240, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_quarantineFFP240 <- counters_quarantine_FFP240 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_quarantine_ffp240 <- calc_qaly_cost(
  agentPS = agentPS_quarantineswabffp240,
  evolution_df = agg_mean_evolution_quarantine_ffp240,
  agg_quarantine = agg_mean_quarantine_quarantineFFP240,
  mask_flag = TRUE,
  quarantine_flag = TRUE,
  testing_flag = TRUE,
  mask_coverage = 0.4,
  cost_per_mask = 1.2,
)

###Scenario quarantena FFP2 90%
counters_quarantine_FFP290 <- lapply(all_runs_quarantineswabffp290, function(run) run$counters)
evolution_list_quarantine_FFP290 <- lapply(all_runs_quarantineswabffp290, function(run) run$evolution)

# Calcola la media giorno per giorno per ogni colonna
agg_mean_evolution_quarantine_FFP290 <- bind_rows(evolution_list_quarantine_FFP290, .id = "run") %>%
  group_by(Day) %>%
  summarise(across(Susceptible:Died, \(x) mean(x, na.rm = TRUE)), .groups = "drop")


agg_mean_quarantine_quarantineFFP290 <- counters_quarantine_FFP290 %>%
  bind_rows() %>%
  group_by(day) %>%
  summarise(mean_quarantine = mean(AGENTS_IN_QUARANTINE,, na.rm = TRUE), .groups = "drop")

stats_quarantine_ffp290 <- calc_qaly_cost(
  agentPS = agentPS_quarantineswabffp290,
  evolution_df = agg_mean_evolution_quarantine_FFP290,
  agg_quarantine = agg_mean_quarantine_quarantineFFP290,
  mask_flag = TRUE,
  quarantine_flag = TRUE,
  testing_flag = TRUE,
  mask_coverage = 0.9,
  cost_per_mask = 1.2,
)



# ---- 1. Lista dei confronti vs baseline ----
# Ogni riga è un confronto tra scenario baseline e scenario con mascherina
df_confronti <- tibble(
  scenario = c("Surgical Mask 40%", "Surgical Mask 90%", "FFP2 Mask 40%", "FFP2 Mask 90%", "Quarantine swab", "Quarantine swab Surgical 40%", "Quarantine swab Surgical 90%", "Quarantine swab FFP2 40%", "Quarantine swab FFP2 90%"),
  delta_qaly = c(
    stats_surgical40$qaly_total - stats_nocont$qaly_total,
    stats_surgical90$qaly_total - stats_nocont$qaly_total,
    stats_FFP240$qaly_total - stats_nocont$qaly_total,
    stats_FFP290$qaly_total - stats_nocont$qaly_total,
    stats_quarantine$qaly_total - stats_nocont$qaly_total,
    stats_quarantine_sur40$qaly_total - stats_nocont$qaly_total,
    stats_quarantine_sur90$qaly_total - stats_nocont$qaly_total,
    stats_quarantine_ffp240$qaly_total - stats_nocont$qaly_total,
    stats_quarantine_ffp290$qaly_total - stats_nocont$qaly_total


  ),
  delta_cost = c(
    stats_surgical40$total_cost - stats_nocont$total_cost,
    stats_surgical90$total_cost - stats_nocont$total_cost,
    stats_FFP240$total_cost - stats_nocont$total_cost,
    stats_FFP290$total_cost - stats_nocont$total_cost,
    stats_quarantine$total_cost - stats_nocont$total_cost,
    stats_quarantine_sur40$total_cost - stats_nocont$total_cost,
    stats_quarantine_sur90$total_cost - stats_nocont$total_cost,
    stats_quarantine_ffp240$total_cost - stats_nocont$total_cost,
    stats_quarantine_ffp290$total_cost - stats_nocont$total_cost

  )
) %>%
  mutate(
    icer = ifelse(delta_qaly > 0, delta_cost / delta_qaly, NA),
    confronto = paste("No measures vs", scenario)
  )

df_pareto <- df_confronti %>%
  arrange(delta_qaly, delta_cost) %>%
  mutate(dominant = !duplicated(cummin(delta_cost)))  # True se non dominata

# Plot pareto
ggplot(df_pareto, aes(x = delta_qaly, y = delta_cost, label = scenario)) +
  geom_point(aes(color = dominant), size = 5) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "red")) +
  labs(
    title = "Cost-effectiveness scatterplot",
    x = "ΔQALY (vs No measures)",
    y = "ΔCost (EUR)",
    color = "Dominant strategy"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


##plotto
ggplot(df_confronti, aes(x = delta_qaly, y = delta_cost, color = scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 4) +
  geom_text(aes(label = scenario), hjust = -0.1, vjust = 0.5, size = 3.5) +
  labs(
    title = "Cost-effectiveness plane: ΔCost vs ΔQALY",
    x = "ΔQALY (scenario - No measures)",
    y = "ΔCost (EUR)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # togli legenda se le etichette bastano
    plot.title = element_text(hjust = 0.5)
  )+
  xlim(-1,3)

