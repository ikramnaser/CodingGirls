setwd("") # indicare nelle virgolette il path dove avete scaricato i dati 
library(readxl)
library(dplyr)
library(tidyverse)
dati <- read_excel("Indicatori_per_regione_sesso.xlsx")

### Scegliere - In questo esempio abbiamo scelto "Totale"
# Maschi
# Femmine
# Totale
dati<-filter(dati, SESSO == "Totale")

### scegleire il dominio - In questo esempio abbimo scelto "Istruzione e formazione"
#Ambiente
#Benessere economico
#Benessere soggettivo
#Innovazione, ricerca e creatività
#Istruzione e formazione
#Lavoro e conciliazione dei tempi di vita
#Paesaggio e patrimonio culturale
#Politica e istituzioni
#Qualità dei servizi
#Relazioni sociali
#Salute
#Sicurezza

dati<-filter(dati, DOMINIO == "Istruzione e formazione")
head(dati)

### Scegliere anno da 2004 a 2022 - In questo esempio abbimo scelto 2020 (andrà sostituito l'anno da voi scelto al posto di 2020 nei comandi) 

## si riporta il reshape del dta set sia mettendo in colonna il codiche degli indicatore che che il nome, scegleirei quello che si preferisce

dati_c<-dati[,c("CODICE","TERRITORIO", "2020")]
dati_c$"2020"<-as.numeric(gsub(",", ".", gsub("\\.", "", dati_c$"2020")))
dati_c<-na.omit(dati_c)
df_wide_c <- dati_c %>% 
  pivot_wider(
    names_from = "CODICE",
    values_from = "2020")

dati_i<-dati[,c("INDICATORE","TERRITORIO", "2020")]
dati_i$"2020"<-as.numeric(gsub(",", ".", gsub("\\.", "", dati_i$"2020")))
dati_i<-na.omit(dati_i)
df_wide_i <- dati_i %>% 
  pivot_wider(
    names_from = "INDICATORE",
    values_from = "2020")


# scegleire quale dei due dataset utilizzare
dati=df_wide_c
dati=df_wide_i
str(dati)
head(dati)

## Rimuovere i missing value

# eliminare colonna con missing rinunciando ad alcuni indicatori 
dati <- dati[colSums(is.na(dati)) == 0]
# eliminare colonne con più di, per esempio, 4 missing se l'indicatore manca solo in lacune, poche, regioni 
# dati <- dati[colSums(is.na(dati)) < 5]
# eliminare righe con missing rinunciando ad alcune regioni 
dati<-na.omit(dati)


## Rimuovere le righe che contengono dati aggregati per zone Nilsen 
dati <- subset(dati, TERRITORIO!='Nord' & TERRITORIO!='Centro' & TERRITORIO!='Sud' & TERRITORIO!='Isole' & TERRITORIO!='Mezzogiorno' & TERRITORIO!='Nord-est' & TERRITORIO!='Nord-ovest' & TERRITORIO!='Italia')

## Aggiungere una colonna con la zona Nilsen corrispondente ad ogni regione 
# editare manualmente i dati per inserire nuove colonne/righe o modificare singole celle
# oppure unire un dataset con le zone (merge) 

#install.packages("DataEditR")
#library(DataEditR)
#dati<- edit(dati)

zone <- read_csv("zone.csv")
zone<-zone[,c("TERRITORIO","Area")]
dati <- merge(dati,zone,by="TERRITORIO")
head(dati)

# salvare il data set in formato Excel 
library("writexl")
write_xlsx(dati, "istruzione.xlsx")




