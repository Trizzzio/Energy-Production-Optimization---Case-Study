##Linear programming


library(lpSolve)

# Parametri modello 
n_ore <- nrow(df)
V_max <- 6700000  # Max Volume in mc
V_min <- 4800000  # Min ´Volume in mc
V_init <- 6100000  # Volume iniziale in mc
F_max <- 31     # Portata massima in derivazione in mc/s
E_max <- 135   # Potenza max in MW
E_coeff <- 1.21  # Coefficiente energetico kWh/m³
delta_t <- 3600  # delta tempo in secondi (1 hour = 3600 seconds)

# Funzione obiettivo 
obj <- df$Prezzo * (E_coeff/1000) 


#Definizione dei Vincoli, e.g. matrici A e b in Ax=b

#Sottmoatrice A1: Vincolo portata massima = Q(t)<= 31*36000

Qmax=F_max*delta_t
A1<-diag(n_ore)
b1<-rep(Qmax,n_ore)

#Sottmoatrice A2: Vincolo portata minima = Q(t)>= 0

Qmin=0
A2<-diag(n_ore)
b2<-rep(Qmin,n_ore)

#Sottomatrice A3: Vincolo Bacino massimo = V(t)<V_max

A3 <- matrix(0, n_ore, n_ore)
for (t in 1:n_ore) {
  for (i in 1:t) {
    A3[t, i] <- 1
  }
}
b3 <- -V_max + V_init + 18000 * 1:n_ore

#Sottomatrice A4: Vincolo Bacino minimo = V(t)<V_min

A4 <- matrix(0, n_ore, n_ore)
for (t in 1:n_ore) {
  for (i in 1:t) {
    A4[t, i] <- 1
  }
}

b4 <- V_init -V_min + 18000 * 1:n_ore

## Definizione della direzione dei vincoli
mdir <- c(rep("<=", n_ore), rep(">=", n_ore), rep(">=", n_ore), rep("<=", n_ore))

#Unione delle matrici

A <- rbind(A1, A2, A3, A4)
b <- c(b1, b2, b3, b4)

# Soluzione usando funzione lp()
soluzione <- lp("max", obj, A, mdir, b)

# Get optimal flows and compute total revenue
flusso_ott <- soluzione$solution
ricavi_totali <- sum(flusso_ott * df$Prezzo * E_coeff/1000)
print(paste("Ricavi totali: €", round(ricavi_totali, 2)))
