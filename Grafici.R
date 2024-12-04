library(tibble)
library(dplyr)
library(ggplot2)
library(scales)

##Grafici per approccio Greedy

#Tibble con dati
dati_ag <- tibble(
  Data = df$Data, 
  Ora = df$Ora,# Colonna data dal dataset df
  Flusso = Flusso*3600,       # Flusso orario in m³/s
  Ricavo = Ricavi        # Ricavo orario in Euro
)

# Aggiornamento della colonna Data per mantenere solo la data (senza ora)
dati_ag <- dati_ag %>%
  mutate(Data = as.Date(Data))

# Somma su base giornaliera
dati_giornalieri_ag <- dati_ag %>%
  group_by(Data) %>%
  summarise(
    Flusso_Giornaliero = sum(Flusso, na.rm = TRUE),        # Somma del flusso giornaliero
    Ricavo_Giornaliero = sum(Ricavo, na.rm = TRUE)         # Somma del ricavo giornaliero
  ) %>%
  mutate(Ricavo_Cumulativo = cumsum(Ricavo_Giornaliero))  # Calcolo del profitto cumulativo giornaliero




last_ricavo <- tail(dati_giornalieri_ag$Ricavo_Cumulativo, n = 1)
last_date <- tail(dati_giornalieri_ag$Data, n = 1)

ggplot(dati_giornalieri_ag, aes(x = Data)) +
  
  # Line for Flusso Giornaliero
  geom_line(aes(y = Flusso_Giornaliero, color = "Flusso Giornaliero (m³)"), linewidth = 1) +  
  
  # Line for Ricavo Giornaliero
  geom_line(aes(y = Ricavo_Cumulativo, color = "Ricavo Cumulativo (Euro)"), linewidth = 1) +
  
  # Add a horizontal dashed line at the last Ricavo_Cumulativo value
  geom_hline(yintercept = last_ricavo, linetype = "dashed", color = "red") +
  
  # Annotate the text with the maximum Ricavo value next to the dashed line
  annotate("text", x = last_date, y = last_ricavo, 
           label = paste("Profitto Finale:", format(last_ricavo, big.mark = ","), "Euro"), 
           vjust = -1, hjust = 1, color = "red", size = 4) +
  
  # Primary and secondary y-axes
  scale_y_continuous(
    name = "Flusso (m³)",  # Label for the primary axis
    labels = label_number(scale = 1, big.mark = "", accuracy = 1),  # Format large numbers for the primary axis
    sec.axis = sec_axis(~., name = "Ricavo Cumulativo (Euro)", labels = label_number(scale = 1, big.mark = "", accuracy = 1))  # Format large numbers for the secondary axis
  ) +
  
  # Customize line colors
  scale_color_manual(values = c("Flusso Giornaliero (m³)" = "blue", "Ricavo Cumulativo (Euro)" = "red")) +
  
  # Add labels and customize theme
  labs(x = "Data", color = "Legenda") +
  theme_minimal() +
  theme(legend.position = "bottom")



##Grafici per PL

dati_pl<-tibble(
  Data = df$Data, 
  Flusso = flusso_ott,       # Flusso orario in m³/s
  Ricavo = flusso_ott*E_coeff/1000*df$Prezzo        # Ricavo orario in Euro
)

dati_giornalieri_pl <- dati_pl %>%
  group_by(Data) %>%
  summarise(
    Flusso_Giornaliero = sum(Flusso, na.rm = TRUE),        # Somma del flusso giornaliero
    Ricavo_Giornaliero = sum(Ricavo, na.rm = TRUE)         # Somma del ricavo giornaliero
  ) %>%
  mutate(Ricavo_Cumulativo = cumsum(Ricavo_Giornaliero))  # Calcolo del profitto cumulativo giornaliero

##Grafico


# Creazione del grafico con Flusso_Giornaliero e Ricavo_Giornaliero
last_ricavo <- tail(dati_giornalieri_pl$Ricavo_Cumulativo, n = 1)
last_date <- tail(dati_giornalieri_pl$Data, n = 1)

ggplot(dati_giornalieri_pl, aes(x = Data)) +
  
  # Line for Flusso Giornaliero
  geom_line(aes(y = Flusso_Giornaliero, color = "Flusso Giornaliero (m³)"), linewidth = 1) +  
  
  # Line for Ricavo Giornaliero
  geom_line(aes(y = Ricavo_Cumulativo, color = "Ricavo Cumulativo (Euro)"), linewidth = 1) +
  
  # Add a horizontal dashed line at the last Ricavo_Cumulativo value
  geom_hline(yintercept = last_ricavo, linetype = "dashed", color = "red") +
  
  # Annotate the text with the maximum Ricavo value next to the dashed line
  annotate("text", x = last_date, y = last_ricavo, 
           label = paste("Profitto Finale:", format(last_ricavo, big.mark = ","), "Euro"), 
           vjust = -1, hjust = 1, color = "red", size = 4) +
  
  # Primary and secondary y-axes
  scale_y_continuous(
    name = "Flusso (m³)",  # Label for the primary axis
    labels = label_number(scale = 1, big.mark = "", accuracy = 1),  # Format large numbers for the primary axis
    sec.axis = sec_axis(~., name = "Ricavo Cumulativo (Euro)", labels = label_number(scale = 1, big.mark = "", accuracy = 1))  # Format large numbers for the secondary axis
  ) +
  
  # Customize line colors
  scale_color_manual(values = c("Flusso Giornaliero (m³)" = "blue", "Ricavo Cumulativo (Euro)" = "red")) +
  
  # Add labels and customize theme
  labs(x = "Data", color = "Legenda") +
  theme_minimal() +
  theme(legend.position = "bottom")



####Grafico di comparazione

dati_c <- tibble(
  Data = dati_giornalieri_ag$Data,
  `Ricavo Cumulativo miopico` = dati_giornalieri_ag$Ricavo_Cumulativo,
  `Ricavo Cumulativo PL` = dati_giornalieri_pl$Ricavo_Cumulativo
)

max_ricavo_miopico <- tail(dati_c$`Ricavo Cumulativo miopico`, n = 1)
max_ricavo_pl <- tail(dati_c$`Ricavo Cumulativo PL`, n = 1)
max_data <- tail(dati_c$Data, n = 1)

# Creazione del grafico
ggplot(dati_c, aes(x = Data)) +
  
  # Linee per i ricavi cumulativi
  geom_line(aes(y = `Ricavo Cumulativo miopico`, color = "Ricavo Miopico"), linewidth = 1) +
  geom_line(aes(y = `Ricavo Cumulativo PL`, color = "Ricavo PL"), linewidth = 1) +
  
  # Linea orizzontale dashed per il massimo Ricavo Miopico
  geom_hline(yintercept = max_ricavo_miopico, linetype = "dashed", color = "blue") +
  # Annotazione del valore massimo Ricavo Miopico
  annotate("text", x = max_data, y = max_ricavo_miopico, 
           label = paste("Max Miopico:", format(max_ricavo_miopico, big.mark = ",")), 
           vjust = -1, hjust = 1, color = "blue", size = 4) +
  
  # Linea orizzontale dashed per il massimo Ricavo PL
  geom_hline(yintercept = max_ricavo_pl, linetype = "dashed", color = "red") +
  # Annotazione del valore massimo Ricavo PL
  annotate("text", x = max_data, y = max_ricavo_pl, 
           label = paste("Max PL:", format(max_ricavo_pl, big.mark = ",")), 
           vjust = -1, hjust = 1, color = "red", size = 4) +
  
  # Personalizzazione delle etichette
  labs(x = "Data", 
       y = "Ricavo Cumulativo (Euro)", 
       title = "Confronto tra Ricavo Miopico e Programmazione Lineare",
       color = "Legenda") +
  
  # Personalizzazione colori delle linee
  scale_color_manual(values = c("Ricavo Miopico" = "blue", "Ricavo PL" = "red")) +
  
  # Formattazione dell'asse y per mostrare valori numerici "normali" (non in notazione scientifica)
  scale_y_continuous(labels = label_number(scale = 1, big.mark = "", accuracy = 1)) +
  
  # Miglioramenti estetici
  theme_minimal() +
  theme(legend.position = "bottom")