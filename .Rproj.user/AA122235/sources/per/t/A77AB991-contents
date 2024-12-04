#Dynamic Programming

# Parametri modello 
n_ore <- nrow(df)
V_max <- 6700000  # Max Volume in mc
V_min <- 4800000  # Min ´Volume in mc
V_init <- 6100000  # Volume iniziale in mc
F_max <- 31     # Portata massima in derivazione in mc/s
E_max <- 135   # Potenza max in MW
E_coeff <- 1.21  # Coefficiente energetico kWh/m³
delta_t <- 3600  # delta tempo in secondi (1 hour = 3600 seconds)

# Tabella per memorizzare i ricavi massimi per ogni stato
# Righe = possibili volumi del serbatoio (in mc), Colonne = ore
dp_table <- matrix(-Inf, nrow = V_max - V_min + 1, ncol = n_ore)

# Inizializziamo il volume di partenza con il volume iniziale
dp_table[V_init - V_min + 1, 1] <- 0  # Ricavo iniziale a 0

# Funzione per calcolare il ricavo orario dato il rilascio di acqua (portata)
calcola_ricavo <- function(portata, prezzo) {
  energia <- portata * E_coeff * delta_t  # kWh
  ricavo <- energia * prezzo  # Ricavo in Euro
  return(ricavo)
}



