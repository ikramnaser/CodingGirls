#load the libraries
library("tidyverse")

#import data
data <- read.csv("C:/Users/ikry/Downloads/archive (4)/drug_deaths.csv")
View(data)



# GENDER DISRIBUTION 

library(dplyr)
library(ggplot2)


data %>% 
  select(Sex) %>% 
  filter(!is.na(Sex), Sex != "Unknown") %>% 
  count(Sex, sort = TRUE) %>%
  mutate(percent = round(n/sum(n) * 100, 0)) %>% 
  filter(percent > 0) %>%  
  ggplot(aes(x = Sex, y = n, label = paste0(percent, "%"), fill = Sex)) +
  geom_bar(stat = "identity") +
  geom_text(col = "black" , vjust = 1.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  theme(legend.position = "none") +
  labs(
    title = "Distribuzione di Genere",
    x = "Genere",
    y = "Count"
  )

#RACE DISTRIBUTION 



dataRace <- data %>% 
  select(Race) %>% 
  filter(!is.na(Race)) %>% 
  group_by(Race) %>%
  summarise(CountRace = n(), .groups = "drop") %>%
  arrange(desc(CountRace)) %>%
  ungroup()

ggplot(dataRace, aes(x = fct_reorder(Race, CountRace), y = CountRace, fill = Race, label = CountRace)) +
  geom_col(width = 0.7) +
  coord_flip() +
  geom_text(hjust = -0.2, color = "steelblue", fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +  
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = expansion(add = c(0,200)))+
  labs(
    title = "distribuzione etnica",
    x = "Etnia",
    y = "Count"
   )





#AGE

dataAge <- data %>% select(ID, Age) %>% 
  filter(!is.na(Age)) %>% mutate(Age = as.numeric(Age)) %>% 
  mutate(AgeGroup = as.factor(cut(Age, breaks = c(14,20,30,49,65,Inf),
                                labels = c("14-20", "21-29", "30-49", "50-65", "Over 65"))))


dataAge %>% filter(!is.na(AgeGroup)) %>% 
  group_by(AgeGroup) %>%
  summarise(Countind = n(), .groups = "drop") %>%
  arrange(desc(Countind)) %>%
  ggplot(aes(x = fct_reorder(AgeGroup, Countind), y = Countind, fill = AgeGroup, label = Countind )) +
  geom_col(width = 0.7) +
  coord_flip() +
  geom_text(hjust = -0.2, color = "steelblue", fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = expansion(add = c(0,200)))+
  labs(
    title = "quale gruppo ha più overdoses?",
    x = "Età",
    y = "Count"
  )


 

#Creiamo un nuovo dataset con solo le colonne che ci interessano 

# definiamo le colonne delle droghe
cols <- c(
  "Heroin", "Cocaine", "Fentanyl", "Fentanyl_Analogue",
  "Oxycodone", "Oxymorphone", "Ethanol", "Hydrocodone",
  "Benzodiazepine", "Methadone", "Amphet", "Tramad",
  "Morphine_NotHeroin", "Hydromorphone", "Other", "OpiateNOS", "AnyOpioid"
)

# nuovo dataset con colonna finale che indica la droga usata (1 se usata, 0 se non usata)
new_data <- data %>%
  select(ID, Date, Age, Sex, Race, COD, all_of(cols)) %>%
  mutate(drug_name = case_when(
    Heroin == 1 ~ "heroin",
    Cocaine == 1 ~ "cocaine",
    Fentanyl == 1 ~ "fentanyl",
    Fentanyl_Analogue == 1 ~ "fentanyl_analogue",
    Oxycodone == 1 ~ "oxycodone",
    Oxymorphone == 1 ~ "oxymorphone",
    Ethanol == 1 ~ "ethanol",
    Hydrocodone == 1 ~ "hydrocodone",
    Benzodiazepine == 1 ~ "benzodiazepine",
    Methadone == 1 ~ "methadone",
    Amphet == 1 ~ "amphetamine",
    Tramad == 1 ~ "tramadol",
    Morphine_NotHeroin == 1 ~ "morphine_not_heroin",
    Hydromorphone == 1 ~ "hydromorphone",
    Other == 1 ~ "other",
    OpiateNOS == 1 ~ "opiate_nos",
    AnyOpioid == 1 ~ "any_opioid",
    TRUE ~ "unknown"
  ))

View(new_data)



# relazione tra droga utilizzata ed etnia


library(dplyr)
library(ggplot2)
library(forcats)


etnie <- c("White", "Black", "Hispanic, Black", "Hispanic, White", "Asian, other", "Other")

# visualizziamo solo le top5
top_n_drugs <- 5

# Filtrare le etnie selezionate
filtered_data <- new_data %>%
  filter(Race %in% etnie)

# ragruppiamo le droghe per etnia e Count
drug_race_counts <- filtered_data %>%
  group_by(Race, drug_name) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Race, desc(Count))

top_drugs_by_race <- drug_race_counts %>%
  group_by(Race) %>%
  top_n(top_n_drugs, Count)

ggplot(top_drugs_by_race, aes(x = fct_reorder(drug_name, Count), y = Count, fill = drug_name)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~Race, scales = "free_y") +
  labs(
    title = "Uso di droghe per etnia",
    x = "Drug Name",
    y = "Count"
  )
