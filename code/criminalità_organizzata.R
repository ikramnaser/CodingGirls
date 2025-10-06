#load the libraries
install.packages("readxl")
library(readxl)

#import datasets
tasso_criminalità <- read_excel("C:/Users/ikry/OneDrive/Desktop/criminalità organizzata/tasso_criminalità.xlsx")
View(tasso_criminalità)

str(tasso_criminalità) #questo è per vedere che dat abbiamo (numeri, string, date...)

tasso_rapine <- read_excel("C:/Users/ikry/OneDrive/Desktop/criminalità organizzata/tasso_rapine.xlsx")
View(tasso_rapine)

str(tasso_rapine)

tasso_omicidi <- read_excel("C:/Users/ikry/OneDrive/Desktop/criminalità organizzata/tasso_omicidi.xlsx")
View(tasso_omicidi)
str(tasso_omicidi)

criminalità_minorile <- read_excel("C:/Users/ikry/OneDrive/Desktop/criminalità organizzata/criminalità_minorile.xlsx")
View(criminalità_minorile)
str(criminalità_minorile)

library(dplyr)
tasso_criminalità <- tasso_criminalità %>%
  mutate(across(-Regione, as.numeric)) #stiamo trasformando in numeri tutte le colonne eccetto la colonna 'Regione'

tasso_rapine <- tasso_rapine %>%
  mutate(across(-Regione, as.numeric))

tasso_omicidi <- tasso_omicidi %>%
  mutate(across(-Regione, as.numeric))

criminalità_minorile <- criminalità_minorile %>%
  mutate(across(-Regione, as.numeric)) 



# nuovo check 
str(tasso_criminalità)
str(tasso_rapine)
str(tasso_omicidi)
str(criminalità_minorile)



library(ggplot2)


criminalità_media <- tasso_criminalità %>%
  filter(Regione != "Bolzano" & Regione != "Trento") %>%  # Escludiamo righe 'Bolzano' e 'Trento'
  group_by(Regione) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  rowwise() %>%
  mutate(average_value = mean(c_across(where(is.numeric)), na.rm = TRUE))

ggplot(criminalità_media, aes(x = Regione, y = average_value, label = round(average_value, 2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(size = 3, vjust = -0.5, color = "black") +  # Add labels on the bars
  labs(title = "Tasso di Criminalità Media 2009-2016", x = "Regione", y = "Criminalità Media") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



average_values <- criminalità_minorile %>%
  filter(Regione != "Bolzano" & Regione != "Trento") %>%  # Escludiamo righe 'Bolzano' e 'Trento'
  group_by(Regione) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  rowwise() %>%
  mutate(average_value = mean(c_across(where(is.numeric)), na.rm = TRUE))


ggplot(average_values, aes(x = Regione, y = average_value, label = round(average_value, 2))) +  
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +  
  labs(x = "Regione", y = "Criminalità minorile media", title = "Criminalità minorile media per Regione") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#note:
#da capire cosa si intende per tasso, perchè Valle d'Aosta ha un tasso di criminalità cosi alto?




#adesso prendiamo come caso studio una regione a scelta (Lomabrdia) e analizziamo l'andamento nel corso degli anni

lombardia_data <- tasso_omicidi %>%
  filter(Regione == "Lombardia")  #fatelo per più regioni per confrontare i risultati 

lombardia_data_long <- tidyr::pivot_longer(lombardia_data, cols = -Regione, names_to = "Year", values_to = "Rate")


ggplot(lombardia_data_long, aes(x = as.numeric(Year), y = Rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Tasso di Omicidi in Lombardia", x = "Anno", y = "Rate") +
  theme_minimal()


lombardia_minori <- criminalità_minorile %>%
  filter(Regione == "Lombardia")  

lombardia_minori_long <- tidyr::pivot_longer(lombardia_minori, cols = -Regione, names_to = "Year", values_to = "Rate")


ggplot(lombardia_minori_long, aes(x = as.numeric(Year), y = Rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Criminalità minorile in Lombardia", x = "Anno", y = "Rate") +
  theme_minimal()


lombardia_rapine <- tasso_rapine %>%
  filter(Regione == "Lombardia")  

lombardia_rapine_long <- tidyr::pivot_longer(lombardia_rapine, cols = -Regione, names_to = "Year", values_to = "Rate")


ggplot(lombardia_rapine_long, aes(x = as.numeric(Year), y = Rate)) +
  geom_line() +
  geom_point() +
  labs(title = "tasso rapine in Lombardia", x = "Anno", y = "Rate") +
  theme_minimal()
