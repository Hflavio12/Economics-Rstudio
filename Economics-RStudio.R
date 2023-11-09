

# Dataset Economics  ------------------------------------------------------


# Carichiamo alcuni pacchetti importanti ----------------------------------
library(tidyverse)
library(ggplot2)


# Punto 1 -----------------------------------------------------------------
# Controlliamo le dimensioni del dataset
dimensione_dataset <-dim(Economics)
print(dimensione_dataset)


# Punto 2 -----------------------------------------------------------------
# Quanti dati sono "ostacolanti" nel dataset Economics? ("N/A"; "-" ed altro) -------------------
Economics_visione <- complete.cases(Economics)  # Questa riga si occupa di creare un vettore ove i "TRUE" sono i valori non "assenti" o corrotti
dati_integrali_Economics <- subset(Economics, Economics_visione) # Questa riga si occupa di creare un nuovo dataset sulla base dei "TRUE" del codice precedente, dunque tutti i valori anomali saranno elusi.
dati_anomali_Economics <- 55-55  #Sottrai le osservazioni totali con le osservazioni senza valori anomali
print(dati_anomali_Economics)  # Stampi i valori anomali

#punto2.5----------------------------------------------------------------------------------------------------
#sono presenti dati anomali (0.0) per le università di roma Sapienza e di Bolzano controllando i dati del USTAT abbiamo controllato i valori ufficiali e li modifichiamo nel dataset
Economics["ROMA - LA SAPIENZA", "P3B"] <- 16.45 
Economics["BOLZANO", "P4B"] <- 18.71

# Punto 3 -----------------------------------------------------------------
# La colonna P1 ci dice quanto è il rateo di iscrizioni nelle università dal primo al secondo anno, maggiore è il valore e più è elevato il numero di iscrizioni dal primo al secondo anno in una specifica città, troviamo il valore ed a che città appartiene.
valore_massimo_colonna_P1 <- max(Economics$P1)  # Trovare l'indice massimo della colonna "P1"
indici_righe <- which(Economics$P1 == valore_massimo_colonna_P1)  # Trovare a che riga appartiene
nomi_città <- rownames(Economics)[indici_righe]  # Trovare il nome della città
cat("Il valore massimo della colonna 'P1' è", valore_massimo_colonna_P1, "\n") # Stampa il nome delle città

for (i in 1:length(nomi_città)) {
  cat("Si trova nella riga di nome:", nomi_città[i], "\n")
}
     


# Punto 4 -----------------------------------------------------------------
# Generiamo un istogramma per il rateo di iscrizioni nelle università dal primo al secondo anno per ogni città
P1_values <- Economics[,1]  # Seleziona la colonna P1

df <- data.frame(Citta = rownames(Economics), P1 = P1_values)   # Crea un dataframe con i dati che servono

ggplot(df, aes(x = Citta, y = P1)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Valori di P1 per città") +
  xlab("Città") +
  ylab("Valore di P1") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Crea il grafico




# Punto 5 -----------------------------------------------------------------
# La colonna "P3B" ci dice qual è il rateo di studenti che sono riusciti a completare il master entro il tempo canonico previsto, valori più alti indicano che in quelle città vi è una maggiore densità di studenti che hanno effettuato medesima manzione.
# Mostriamo la città a cui è associato il valore maggiore
valore_massimo_colonna_P3B <- max(Economics$P3B)  # Trovare l'indice massimo della colonna P3B
indice_rigap3 <- which.max(Economics$P3B)  # Trovare a che riga appartiene
nome_città2 <- rownames(Economics)[indice_rigap3]  # Trovare il nome della città
cat(paste("Il valore massimo della colonna 'P3B' è ", valore_massimo_colonna_P3B, "e si trova nella riga di nome: ", nome_città2)) # Stampa il nome delle città
 


# Punto 6 -----------------------------------------------------------------
# La variabile "P4A" ci dice quanti studenti hanno completato il corso triennale di laurea in tempo, vogliamo vedere se questi dati seguono una distribuzione di tipo normale attraverso il Test di Shapiro-Francia. Questo ci servirà ad esempio per verificare la media e la deviazione standard della variabile, ed anche per tracciare un grafico.
colonna_P4A <- Economics$P4A
lunghezza_della_colonna_P4A <- length(colonna_P4A)
colonna_P4A <- sort(colonna_P4A)
y <- qnorm(((1:lunghezza_della_colonna_P4A)-3/8)/(lunghezza_della_colonna_P4A+1/4))
W <- cor(colonna_P4A,y)^2
W

# Punto 7 -----------------------------------------------------------------
# Calcolo Media della variabile "P4A"
media_P4A <- mean(Economics$P4A) * ((lunghezza_della_colonna_P4A-1)/(lunghezza_della_colonna_P4A))
media_P4A

# Punto 8 -----------------------------------------------------------------
deviazione_standard_P4A <- sd(Economics$P4A) * ((lunghezza_della_colonna_P4A-1)/(lunghezza_della_colonna_P4A))
deviazione_standard_P4A



# Ora creiamo un grafico della distribuzione della variabile "P4A"
dataframe_con_P4A <- data.frame(P4A = Economics$P4A)

media_P4A <- mean(dataframe_con_P4A$P4A)
deviazione_standard_P4A <- sd(dataframe_con_P4A$P4A)
colonna_P4A <- dataframe_con_P4A$P4A

dataframe_con_P4A %>%
  ggplot(aes(x = colonna_P4A)) +
  geom_density(fill="lightblue", alpha=0.5) + 
  geom_vline(aes(xintercept=media_P4A, color="Media"), linetype="dashed", linewidth = 1) + 
  geom_vline(aes(xintercept = media_P4A + deviazione_standard_P4A, color="Deviazione Standard"), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept = media_P4A - deviazione_standard_P4A, color="Deviazione Standard"), linetype="dashed", size=1) + 
  scale_color_manual(values=c("orange", "red"), labels=c("Deviazione Standard", "Media")) +
  labs(x="Valore", y="Densità") + 
  ggtitle("Distribuzione normale con media e deviazione standard")

# punto 9(B) -------------------------------------------------------------------------------------------------
#creiamo un grafico con la libreria ploty che ci permette di confrotare il numero di studenti iscritti alla laurea magistrale e gli effettivi laureati 

# Installiamo la libreria plotly
install.packages("plotly")

# Carichiamo la libreria plotly
library(plotly)

# Selezioniamo le colonne P3B e P4B dal dataset Economics
P3B_values <- Economics[, "P3B"]
P4B_values <- Economics[, "P4B"]

# Creiamo un nuovo data con i dati selezionati e assegnamo il nome di ogni città come indice
dfB <- data.frame(Citta = rownames(Economics), P3B = P3B_values, P4B = P4B_values)

# Creiamo un grafico a barre con la libreria plotly. Specificando la variabile x come "Citta", e le altre due una per gli iscritti (grigio) e una per i laureati (arancione).

fig <- plot_ly(dfB, x = ~Citta) %>%
  add_trace(y = ~P3B, name = "iscritti", type = "bar", marker = list(color = "gray")) %>%
  add_trace(y = ~P4B, name = "laureati", type = "bar") %>%
  
  # Aggiungi un titolo al grafico 
  layout(title = "Confronto tra gli iscritti alla magistrale e i laureati per città",
         xaxis = list(title = "Città"),
         yaxis = list(title = "Valore"))

# Mostra il grafico
fig

#Punto 9(A)---------------------------------------------------------------------------------------------------
#ripetiamo il procedimento per confrontare il percorso di laurea triennale

library(plotly)

# Seleziona le colonne P3A e P4A
P3A_values <- Economics[, "P3A"]
P4A_values <- Economics[, "P4A"]

# Crea un dataframe con i dati che servono
dfA <- data.frame(Citta = rownames(Economics), P3A = P3A_values, P4A = P4A_values)

# Crea un grafico a barre con la libreria plotly
fig <- plot_ly(dfA, x = ~Citta) %>%
  add_trace(y = ~P3A, name = "iscritti", type = "bar",marker = list(color="pink")) %>%
  add_trace(y = ~P4A, name = "laureati", type = "bar",marker = list(color="purple")) %>%
  layout(title = "Confronto tra gli iscritti alla triennale e i laureati  per città",
         xaxis = list(title = "Città"),
         yaxis = list(title = "Valore"))

fig

#punto 10----------------------------------------------------------------------------------------------------------------
# grafico a torte della percentuale di università private e publiche presenti in Italia
library(ggplot2)
library(scales)

# Calcoliamo la frequenza delle università pubbliche e private
freq_universita <- table(Economics$University_Type)

# Creiamo un dataframe per il grafico
df1 <- data.frame(Universita = names(freq_universita), Frequenza = as.numeric(freq_universita))

# Creiamo il grafico a torta con le percentuali
ggplot(df1, aes(x = "", y = Frequenza, fill = Universita)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start=0) +
  geom_label(aes(label = percent(Frequenza/sum(Frequenza))), 
             position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Tipo di università") +
  ggtitle("percentuali delle università pubbliche e private presenti in Italia")


#punto 11-----------------------------------------------------------------------------------------------------------------
#creiamo un grafico a barre che confronta la media in percentuale tra pubblica e privata degli studenti che sono riusciti a laurearsi 

# Caricamento della libreria plotly
library(plotly)

# Creazione del dataset con la media delle percentuali di studenti laureati alle università pubbliche e private
df2 <- data.frame(
  University_Type = c("Pubbliche", "Private"),
  mean_P3A = c(mean(Economics$P3A[Economics$University_Type == "Public"]), mean(Economics$P3A[Economics$University_Type == "Private"]))
)

# Creazione del grafico a barre
fig <- plot_ly(df2, x = ~University_Type, y = ~mean_P3A, type = 'bar',
               marker = list(color = c("blue", "red")), # assegnazione dei colori alle barre
               text = ~paste("Media: ", mean_P3A)) %>%
  layout(title = "Confronto della percentuale media di studenti laureati alle università pubbliche e private", # titolo del grafico
         xaxis = list(title = "Tipo di università"), # titolo dell'asse x
         yaxis = list(title = "Media P3A")) # titolo dell'asse y

# Visualizzazione del grafico
fig

#punto 12-----------------------------------------------------------------------------------------------------------------
#TASSO LAUREATI MAGISTRALI E TRIEANNLI NELLE CORRISPETTIVE UNIVERSITA
library(plotly)

# Selezioniamo le colonne P4A e P4B
P4a_values <- Economics[, "P4A"]
P4b_values <- Economics[, "P4B"]

# Creiamo un dataframe con i dati che servono
df3 <- data.frame(Citta = rownames(Economics), P4A = P4a_values, P4B = P4b_values)

# Creiamo un grafico a barre con la libreria plotly
plot_ly(df3, x = ~Citta, y = ~P4A, name = "Tasso di laureati triennali", type = "bar", 
        marker = list(color = "blue")) %>%
  # Aggiungiamo una seconda serie di dati
  add_trace(y = ~P4B, name = "Tasso di laureati magistrali", type = "bar",
            marker = list(color = "orange")) %>%
  # Aggiungiamo titolo e label degli assi
  layout(title = "Confrontiamo tra il tasso di laureati nei corsi triennali e magistrali per città",
         xaxis = list(title = "Città", tickangle = 45),
         yaxis = list(title = "Tasso di laureati", grid = TRUE),
         # Impostiamo il tipo di grafico a barre raggruppate e la distanza tra le barre
         barmode = "group",
         bargap = 0.2)


# Punto 13 ----------------------------------------------------------------
# La colonna D6 ci dice il numero di attività di insegnamento monitorate valutate sul totale delle attività di insegnamento, in altre parole quanto attività di insegnamento sono state monitorate sulle totali 
# Possiamo fare un istogramma delle città per vedere dove questo valore (o valori) è più elevato.
D6_index <- match("D6", names(Economics)) #Troviamo l'indice ove vi è la colonna "D6"
D6_values <- Economics[, 12]


df_per_istogramma_su_D6 <- data.frame(Citta = rownames(Economics), D6 = Economics[, D6_index]) # Crea un dataframe con i dati che servono

# Trova il valore massimo
massimo <- max(D6_values)
print(massimo)

# Crea il grafico, colorando di rosso le barre con valori di D6 superiori a 99, quindi i valori massimi trovati prima
ggplot(df_per_istogramma_su_D6, aes(x=Citta, y=D6, fill=ifelse(D6>99.99, "Rosso", "Viola"))) +
  geom_bar(stat="identity") +
  ggtitle("Valore di D6 per città") +
  xlab("Città") +
  ylab("Valore di D6") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_fill_manual(values=c("Rosso"="red", "Viola"="purple")) + guides(fill="none")

# Stampa i nomi delle città con valori di D6 massimi
cat("Città con valori di D6 massimi:\n")
cat(paste(subset(df_per_istogramma_su_D6, D6>99.99)$Citta, ":", subset(df_per_istogramma_su_D6, D6>99.99)$D6, "\n"))

# Punto 14 ----------------------------------------------------------------
# La colonna D1 esprime numericamente la relazione tra il numero di professori permanenti valutato sul numero dei crediti acquisiti da essi durante gli ultimi due anni.
# Crea un dataframe con la colonna "D1"
print(Economics$D1)


df4 <- data.frame(D1 = c(0.07, 0.07 ,0.09, 0.07 ,0.08, 0.07 ,0.07 ,0.07 ,0.07 ,0.09 ,0.06, 0.11 ,0.07 ,0.07, 0.06, 0.06, 0.08,
                        0.08, 0.07 ,0.07, 0.07, 0.06, 0.05, 0.08, 0.07, 0.07, 0.08, 0.08, 0.10, 0.10, 0.08, 0.08, 0.04, 0.09,
                        0.11 ,0.14, 0.07, 0.08, 0.05, 0.09 ,0.07 ,0.08 ,0.06 ,0.08 ,0.09 ,0.07 ,0.07 ,0.08 ,0.15, 0.10 ,0.14, 0.45, 0.15, 0.32 ,0.16
))

# Suddivide i dati in 5 classi
df4$classe_D1 <- cut(df4$D1, breaks = c(0.04, 0.1, 0.2, 0.3, 0.4, 0.45), include.lowest = TRUE)

# Crea l'istogramma con le frequenze assolute
ggplot(df4, aes(x = classe_D1)) +
  stat_count(geom = "bar", color = "black", fill = "white") +
  ggtitle("Istogramma delle frequenze assolute di D1 suddiviso in 5 classi") +
  xlab("Classe di D1") +
  ylab("Frequenza assoluta")


