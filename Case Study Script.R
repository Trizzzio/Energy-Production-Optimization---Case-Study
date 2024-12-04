#### Case Study Alperia

library(lubridate)
library(tidyverse)
library(dplyr)
library(readxl)


##Uploading and cleaning Datafile 

column <- c("Data","Ora", "Prezzo","Apporto")
df <- read_excel("dfCaseStudy.xlsx",col_names = column, skip = 2)

df$Data <- as.Date(df$Data)
df$Ora <- sprintf("%02d:00:00", df$Ora)  

##Constanti

V_max <- 6700000  # Max Volume in mc
V_min <- 4800000  # Min ´Volume in mc
V_init <- 6100000  # Volume iniziale in mc
F_max <- 31     # Portata massima in derivazione in mc/s
E_max <- 135   # Potenza max in MW
E_coeff <- 1.21  # Coefficiente energetico kWh/m³
delta_t <- 3600  # delta tempo in secondi (1 hour = 3600 seconds)

##Inizializzazione Variabili

n_ore <- nrow(df)  # numero ore
V <- numeric(n_ore)  # riserva ogni ora
Flusso <- numeric(n_ore)  # fluss per ora
Energia <- numeric(n_ore)  # energia prodotta / ora
Ricavi <- numeric(n_ore)  # richavi / ora

V[1] <- V_init  # Set initial volume

# Loop
for (t in 1:(n_ore-1)) {
  #Calcolo flusso max in base a vincoli bacino
  max_flusso <- min(F_max, (V[t] - V_min) / delta_t)
  
  #Determinare flusso che massimizza ricavi
  #"Greedy approach": massimizzare flusso quando prezzo è alto
  Flusso[t] <- ifelse(df$Prezzo[t] > median(df$Prezzo), max_flusso, 0)
  
  # Calcolare energia prodotta in base al flusso 
  Energia[t] <- Flusso[t]*(3600) * E_coeff/1000
  
  # Calolare Profitto/h
  Ricavi[t] <- Energia[t] * df$Prezzo[t]
  
  # Update volume bacino 
  V[t+1] <- V[t] + (df$Apporto[t] - Flusso[t]) * delta_t
  
  # Volume deve stare nei vincoli
  V[t+1] <- max(min(V[t+1], V_max), V_min)
}

# Somma Profitto
Profitto_totale <- sum(Ricavi)

# Output riultati
print(paste("Ricavi totatli: €", round(Profitto_totale, 2)))



