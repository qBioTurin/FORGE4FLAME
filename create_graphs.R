library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)



##simulation for single agents

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

seeds_no_counter <- list.dirs("Hospital_NoCountermeasures", recursive = FALSE)

all_runs_nomask <- lapply(seeds_no_counter, function(seed) {

  # Percorsi dei file nella cartella
  agent_file <- file.path(seed, "AGENT_POSITION_AND_STATUS.csv")
  evo_file   <- file.path(seed, "evolution.csv")

  # Leggi i file
  df_agent <- read_csv(agent_file, show_col_types = FALSE)
  colnames(df_agent) <- c("time", "id", "agent_type", "x", "y", "z", "disease_state")

  df_evo <- read_csv(evo_file, show_col_types = FALSE)

  # Ritorna una lista con i due dataframe
  list(agentPS = df_agent, evolution = df_evo, seed = seed)
})

colnames(all_runs_nomask[[1]]$evolution)

agentPS <- lapply(all_runs_nomask, function(run) run$agentPS)


#agent_with_time_window <- Filter(function(x) x$entry_type == "Time window", canvasObjects$agents)
#agent_with_daily_rate<- Filter(function(x) x$entry_type == "Daily Rate", canvasObjects$agents)
agent_with_time_window <- agents_df %>% filter(agents_df$entry_type == "Time window")
agent_with_daily_rate<- agents_df %>% filter(agents_df$entry_type == "Daily Rate")
agents <- c(agent_with_time_window$type, agent_with_daily_rate$type)
agents
disease_name = c("Susceptible", "Exposed", "Infected", "Recovered", "Died")
disease_name

fixed_colors <- c("Susceptible" = "green", "Exposed" = "blue", "Infected" = "red", "Recovered" = "purple", "Died" = "black")


processedAPS <- lapply(agentPS, function(simulation_log) {
# 1) day
simulation_log <- simulation_log %>%
  mutate(day = ceiling(time / 1440))

# 2) sostituisci numeri con nomi agenti e nomi stati
simulation_log <- simulation_log %>%
  mutate(agent_type = agents[as.numeric(agent_type)+1],
         disease_state = disease_name[as.numeric(disease_state)+1])


global_min_day <- min(simulation_log$day, na.rm = TRUE)
global_max_day <- max(simulation_log$day, na.rm = TRUE)
all_days <- seq(global_min_day, global_max_day)

simulation_log_full <- simulation_log %>%
  arrange(id, day, time) %>%
  group_by(id, agent_type) %>%
  tidyr::complete(day = seq(global_min_day, max(day, na.rm = TRUE))) %>%
  arrange(id, day, time) %>%
  tidyr::fill(disease_state, .direction = "down") %>%  # propagazione ultimo stato
  ungroup()

# 4) ora aggrego per agent_type, disease_state, day
daily_summary <- simulation_log_full %>%
  filter(!(is.na(disease_state))) %>%
  group_by(agent_type, disease_state, day) %>%
  summarise(count = n_distinct(id), .groups = "drop")


daily_complete <- daily_summary %>%
  ungroup() %>%
  mutate(disease_state = factor(disease_state, levels = names(fixed_colors))) %>%
  # complete su agent_type, disease_state e day; riempi count con 0 dove mancante
  complete(agent_type, disease_state = disease_state, day = all_days, fill = list(count = 0)) %>%
  arrange(agent_type, disease_state, day) %>%
  mutate(count = as.integer(count))   # forziamo integer

seen_pairs <- daily_summary %>% distinct(agent_type, disease_state)
daily_complete <- daily_complete %>% semi_join(seen_pairs, by = c("agent_type","disease_state"))

})






if(length(all_runs_nomask) != length(processedAPS)) {
  warning("ATTENZIONE: processedAPS e all_runs_nomask hanno lunghezze diverse!")
}

# Aggiorna agentPS run-by-run
for(i in seq_along(all_runs_nomask)) {
  if(is.null(all_runs_nomask[[i]]$agentPS)) {
    warning(paste("Run", i, "ha agentPS NULL. Skip."))
    next
  }
  if(i > length(processedAPS) || is.null(processedAPS[[i]])) {
    warning(paste("Run", i, "non ha processedAPS corrispondente. Skip."))
    next
  }
  # Assegna il dataframe aggiornato
  all_runs_nomask[[i]]$agentPS <- processedAPS[[i]]
}





bb <- processedAPS[[45]]


pl = ggplot(processedAPS[[45]], aes(x = day, y = count, color = factor(disease_state), group = disease_state)) +
  geom_line()+
  facet_wrap(~agent_type, scales = "free_y") +
  expand_limits(y = 1) +
  scale_color_manual(values = fixed_colors)
pl


agg_mean_area <- processedAPS %>%
  bind_rows() %>%
  filter(!is.na(agent_type)) %>%
  group_by(agent_type, disease_state, day) %>%
  summarise(
    MinV  = min(count, na.rm = TRUE),
    MaxV  = max(count, na.rm = TRUE),
    MeanV = mean(count, na.rm = TRUE),
    .groups = "drop"
  )

p_area <- ggplot() +
  geom_line(data = processedAPS[[1]],
            aes(x = day, y = count, color = factor(disease_state), group = disease_state),
            size = 0.5) +
  # area min-max
  geom_ribbon(data = agg_mean_area, aes(x = day, ymin = MinV, ymax = MaxV, fill = disease_state), alpha = 0.25, colour = NA) +
  facet_wrap(~ agent_type, scales = "free_y") +
  scale_fill_manual(values = fixed_colors, name = "Stato") +
  labs(x = "Giorno", y = "Numero agenti", title = "Min–Max areas") +
  scale_color_manual(values = fixed_colors)
  theme(legend.position = "bottom")
p_area


p_mean <- ggplot() +
  #geom_line(data = processedAPS[[1]],
            # aes(x = day, y = count, color = factor(disease_state), group = disease_state),
            # size = 0.5) +
  geom_line(data = agg_mean_area, aes(x = day, y = MeanV, color = disease_state), size = 1, linetype = "dashed") +
  facet_wrap(~ agent_type, scales = "free_y") +
  scale_color_manual(values = fixed_colors, name = "Media") +
  labs(x = "Giorno", y = "Numero agenti", title = "Min–Max area + Media across sims") +
  theme_minimal() +
  theme(legend.position = "bottom")
p_mean

#number of patient for beds

roomInCanvas <- model[["roomsINcanvas"]]

roomsBedroomMedicine <- subset(roomInCanvas, type =="bedroom" & area == "General_Medicine")

bed_weights <- c(
  "one_bed" = 1,
  "two_bed" = 2,
  "three_beds" = 3,
  "four_beds" = 4
)

roomsBedroomMedicine <- roomsBedroomMedicine %>%
  mutate(
    base_name = str_extract(Name, "^(two_bed|three_beds|four_beds|one_bed)"),
    # assegna il peso
    weight = bed_weights[base_name]
  )

sum(roomsBedroomMedicine$weight, na.rm = TRUE)

#CEA
#fare la cumulativa della curva dei pazienti e fare quella per N (dove N è il costo del paziente infetto). Questo è il QALY
#Willingness to Pay (WTP):
#the ICER is often compared to a "willingness to pay" threshold. If the ICER is less than the WTP, the intervention is considered cost-effective.

# fare scenari: confrontare per cea caso base senza contromisure e i 4 delle mascherine, singolarmente
# confrontare quarantena con quello senza contromisure
# confrontare quarantena e quarantena più mascherina 4 tipi

# chiedere costi di mascherina e quarantena a chatgpt, costi medi in italia

seeds_chirur40 <-  list.dirs("Hospital_Surgical40", recursive = FALSE)


all_runs_chirur40 <- lapply(seeds_chirur40, function(seed) {

  # Percorsi dei file nella cartella
  agent_file <- file.path(seed, "AGENT_POSITION_AND_STATUS.csv")
  evo_file   <- file.path(seed, "evolution.csv")

  # Leggi i file
  df_agent <- read_csv(agent_file, show_col_types = FALSE)
  colnames(df_agent) <- c("time", "id", "agent_type", "x", "y", "z", "disease_state")

  df_evo <- read_csv(evo_file, show_col_types = FALSE)

  # Ritorna una lista con i due dataframe
  list(agentPS = df_agent, evolution = df_evo, seed = seed)
})

agentPS_chirur40 <- lapply(all_runs_chirur40, function(run) run$agentPS)

processedAPS_chirur40 <- lapply(agentPS_chirur40, function(simulation_log) {
  # 1) day
  simulation_log <- simulation_log %>%
    mutate(day = ceiling(time / 1440))

  # 2) sostituisci numeri con nomi agenti e nomi stati
  simulation_log <- simulation_log %>%
    mutate(agent_type = agents[as.numeric(agent_type)+1],
           disease_state = disease_name[as.numeric(disease_state)+1])


  global_min_day <- min(simulation_log$day, na.rm = TRUE)
  global_max_day <- max(simulation_log$day, na.rm = TRUE)
  all_days <- seq(global_min_day, global_max_day)

  simulation_log_full <- simulation_log %>%
    arrange(id, day, time) %>%
    group_by(id, agent_type) %>%
    tidyr::complete(day = seq(global_min_day, max(day, na.rm = TRUE))) %>%
    arrange(id, day, time) %>%
    tidyr::fill(disease_state, .direction = "down") %>%  # propagazione ultimo stato
    ungroup()

  # 4) ora aggrego per agent_type, disease_state, day
  daily_summary <- simulation_log_full %>%
    filter(!(is.na(disease_state))) %>%
    group_by(agent_type, disease_state, day) %>%
    summarise(count = n_distinct(id), .groups = "drop")


  daily_complete <- daily_summary %>%
    ungroup() %>%
    mutate(disease_state = factor(disease_state, levels = names(fixed_colors))) %>%
    # complete su agent_type, disease_state e day; riempi count con 0 dove mancante
    complete(agent_type, disease_state = disease_state, day = all_days, fill = list(count = 0)) %>%
    arrange(agent_type, disease_state, day) %>%
    mutate(count = as.integer(count))   # forziamo integer

  seen_pairs <- daily_summary %>% distinct(agent_type, disease_state)
  daily_complete <- daily_complete %>% semi_join(seen_pairs, by = c("agent_type","disease_state"))

})


if(length(all_runs_chirur40) != length(processedAPS_chirur40)) {
  warning("ATTENZIONE: processedAPS e all_runs_nomask hanno lunghezze diverse!")
}

# Aggiorna agentPS run-by-run
for(i in seq_along(all_runs_chirur40)) {
  if(is.null(all_runs_chirur40[[i]]$agentPS)) {
    warning(paste("Run", i, "ha agentPS NULL. Skip."))
    next
  }
  if(i > length(processedAPS_chirur40) || is.null(processedAPS_chirur40[[i]])) {
    warning(paste("Run", i, "non ha processedAPS corrispondente. Skip."))
    next
  }
  # Assegna il dataframe aggiornato
  all_runs_chirur40[[i]]$agentPS <- processedAPS_chirur40[[i]]
}

# ---- PARAMETRI ----
mask_coverage <- 0.40
cost_per_mask <- 0.5         # €/mascherina al giorno
cost_per_infected <- 300.0   # €/persona
u_infected <- 0.65           # utilità quando infetto
days_in_model <- 105          # durata simulazione
mask_start_day <- 18          # giorno in cui le mascherine iniziano ad essere usate

# ---- FUNZIONE AGGIORNATA ----
process_all_runs <- function(all_runs, mask_flag = FALSE) {

  stats_per_run <- lapply(all_runs, function(run) {
    df_agent <- run$agentPS
    df_evo   <- run$evolution
    seed     <- run$seed

    # ---- Estrai pazienti infetti ----
    patients_inf <- df_agent %>%
      filter(tolower(agent_type) == "patient") %>%
      filter(tolower(disease_state) == "infected") %>%
      select(day, infected = count)

    # ---- Usa evolution per total_agents_day ----
    df_combined <- df_evo %>%
      rename(day = "Day") %>%
      mutate(total_agents_day = Susceptible + Exposed + Infected + Recovered + Died) %>%
      left_join(patients_inf, by = "day") %>%
      mutate(
        infected = replace_na(infected, 0),
        noninfected = total_agents_day - infected,
        noninfected = ifelse(noninfected < 0, 0, noninfected),
        qaly_day = (noninfected*1 + infected*u_infected)/days_in_model,
        masks_today = ifelse(mask_flag & day >= mask_start_day, total_agents_day * mask_coverage, 0)
      )

    # ---- Riepilogo per run ----
    df_stats <- df_combined %>%
      summarise(
        qaly_total = sum(qaly_day, na.rm = TRUE),
        est_unique_infected = sum(infected, na.rm = TRUE),
        masks_cost = sum(masks_today * cost_per_mask, na.rm = TRUE),
        cost_infections = est_unique_infected * cost_per_infected,
        total_cost = cost_infections + masks_cost
      )

    return(df_stats)
  })

  # Combina tutti i run
  stats_df <- bind_rows(stats_per_run)
  return(stats_df)
}

# ---- 1. PROCESSA SCENARI ----
stats_no_mask <- process_all_runs(all_runs_nomask, mask_flag = FALSE)
stats_mask40  <- process_all_runs(all_runs_chirur40, mask_flag = TRUE)

# ---- 2. ALLINEAMENTO RUN PER RIGA ----
n_run <- min(nrow(stats_no_mask), nrow(stats_mask40))
stats_no_mask <- stats_no_mask[1:n_run, ]
stats_mask40  <- stats_mask40[1:n_run, ]

# ---- 3. CALCOLO ICER RIGA-PER-RIGA ----
icer_runs <- tibble(
  qaly_total_no   = stats_no_mask$qaly_total,
  total_cost_no   = stats_no_mask$total_cost,
  qaly_total_mask = stats_mask40$qaly_total,
  total_cost_mask = stats_mask40$total_cost
) %>%
  mutate(
    delta_qaly = qaly_total_mask - qaly_total_no,
    delta_cost = total_cost_mask - total_cost_no,
    icer = ifelse(delta_qaly > 0, delta_cost / delta_qaly, NA)
  )

# ---- 4. STATISTICHE MEDIA ± SD ----
summary_stats <- icer_runs %>%
  summarise(
    n_runs = n(),
    delta_qaly_mean = mean(delta_qaly, na.rm = TRUE),
    delta_qaly_sd   = sd(delta_qaly, na.rm = TRUE),
    delta_cost_mean = mean(delta_cost, na.rm = TRUE),
    delta_cost_sd   = sd(delta_cost, na.rm = TRUE),
    icer_mean       = mean(icer, na.rm = TRUE),
    icer_sd         = sd(icer, na.rm = TRUE)
  )

# Aggiungiamo una colonna per lo status ICER
icer_runs_plot <- icer_runs %>%
  mutate(
    ic_status = case_when(
      is.na(icer) ~ "ΔQALY ≤ 0 (NA)",
      icer > 0   ~ "ICER positivo",
      icer < 0   ~ "ICER negativo"
    )
  )

# Calcolo media ΔQALY e ΔCost
icer_means <- icer_runs_plot %>%
  summarise(
    delta_qaly_mean = mean(delta_qaly, na.rm = TRUE),
    delta_cost_mean = mean(delta_cost, na.rm = TRUE)
  )

# Grafico
ggplot(icer_runs_plot, aes(x = delta_qaly, y = delta_cost, color = ic_status)) +
  geom_point(size = 3, alpha = 0.6) +                        # run individuali
  geom_point(data = icer_means, aes(x = delta_qaly_mean, y = delta_cost_mean),
             color = "black", size = 5, shape = 18) +         # med

# Come leggere questo grafico
#
# Ogni punto colorato → singola run
#
# Punto nero → media ΔQALY e ΔCost su tutte le run → rappresenta l’andamento “tipico”
#
# Linee tratteggiate x=0 e y=0 → separano quadranti favorevoli/sfavorevoli
#
# Quadranti come prima:
#
#   Quadrante	Interpretazione
# Alto a destra	ICER positivo → mascherine più efficaci ma più costose
# Alto a sinistra	Dominato → peggiorano QALY e costano di più
# Basso a destra	Dominante → migliorano QALY e risparmiano
# Basso a sinistra	Peggiorano QALY ma costano meno
#

# ---- 5. STAMPA RISULTATI ----
cat("\n--- RIEPILOGO ICER ---\n")
cat(sprintf("N run: %d\n", summary_stats$n_runs))
cat(sprintf("ΔQALY (media ± sd): %.5f ± %.5f\n", summary_stats$delta_qaly_mean, summary_stats$delta_qaly_sd))
cat(sprintf("ΔCost (media ± sd): %.2f ± %.2f EUR\n", summary_stats$delta_cost_mean, summary_stats$delta_cost_sd))
cat(sprintf("ICER (media ± sd): %.2f ± %.2f EUR/QALY\n", summary_stats$icer_mean, summary_stats$icer_sd))



###QALY SULLE MEDIE


# ---- 1. Creare dataframe medi per ogni scenario ----
agg_mean_no_mask <- processedAPS %>%
  bind_rows() %>%
  group_by(agent_type, disease_state, day) %>%
  summarise(mean_count = mean(count, na.rm = TRUE), .groups = "drop")

agg_mean_mask40 <- processedAPS_chirur40 %>%
  bind_rows() %>%
  group_by(agent_type, disease_state, day) %>%
  summarise(mean_count = mean(count, na.rm = TRUE), .groups = "drop")

# ---- 2. Parametri ----
mask_coverage <- 0.4
cost_per_mask <- 0.5
cost_per_infected <- 300
u_infected <- 0.65
days_in_model <- max(agg_mean_no_mask$day)   # 105 o numero massimo dei giorni
mask_start_day <- 18

# ---- 3. Funzione per calcolare QALY e costi ----
calc_qaly_cost <- function(df_mean, mask_flag = FALSE) {

  df_qc <- df_mean %>%
    filter(agent_type == "Patient") %>%   # solo pazienti
    tidyr::complete(day = seq(min(day), max(day)), fill = list(mean_count = 0)) %>%
    mutate(
      infected = ifelse(disease_state == "Infected", mean_count, 0),
      noninfected = sum(mean_count) - infected,
      noninfected = ifelse(noninfected < 0, 0, noninfected),
      qaly_day = (noninfected*1 + infected*u_infected)/days_in_model,
      masks_today = ifelse(mask_flag & day >= mask_start_day, sum(mean_count) * mask_coverage, 0)
    )

  qaly_total <- sum(df_qc$qaly_day, na.rm = TRUE)
  est_unique_infected <- sum(df_qc$infected, na.rm = TRUE)
  masks_cost <- sum(df_qc$masks_today * cost_per_mask, na.rm = TRUE)
  cost_infections <- est_unique_infected * cost_per_infected
  total_cost <- masks_cost + cost_infections

  tibble(qaly_total = qaly_total,
         total_cost = total_cost)
}

# ---- 4. Calcolare stats per ogni scenario ----
stats_mean_no_mask <- calc_qaly_cost(agg_mean_no_mask, mask_flag = FALSE)
stats_mean_mask40  <- calc_qaly_cost(agg_mean_mask40, mask_flag = TRUE)

# ---- 5. Calcolare ICER ----
delta_qaly <- stats_mean_mask40$qaly_total - stats_mean_no_mask$qaly_total
delta_cost <- stats_mean_mask40$total_cost - stats_mean_no_mask$total_cost
icer_mean  <- ifelse(delta_qaly > 0, delta_cost / delta_qaly, NA)

cat(sprintf("ΔQALY: %.5f\nΔCost: %.2f EUR\nICER: %.2f EUR/QALY\n",
            delta_qaly, delta_cost, icer_mean))

# ---- 6. Grafico ΔCost vs ΔQALY ----
df_plot <- tibble(
  delta_qaly = delta_qaly,
  delta_cost = delta_cost
)

ggplot(df_plot, aes(x = delta_qaly, y = delta_cost)) +
  geom_point(color = "black", size = 5, shape = 18) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "ΔCost vs ΔQALY - Media delle simulazioni",
    x = "ΔQALY (mascherina - no mascherina)",
    y = "ΔCost (EUR)",
    subtitle = sprintf("ICER medio: %.2f EUR/QALY", icer_mean)
  ) +
  theme_minimal(base_size = 14)
