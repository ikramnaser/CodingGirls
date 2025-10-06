#load the libraries
library(tidyverse)


#import data
data <- read.csv("C:/Users/ikry/Downloads/archive (5)/world-happiness-report-2021.csv")
View(data)


#top 10 paesi più felici e top 10 paesi più a disagio

library(ggplot2)


top_10_felici <- head(data, 10)
top_10_infelici <- tail(data, 10)

# Plot top 10 happiest countries
ggplot(top_10_felici, aes(x = reorder(`Country.name`, `Ladder.score`), y = `Ladder.score`, fill = `Ladder.score`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`Ladder.score`, 2)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = "Country", y = "Ladder Score", title = "Top 10 paesi più felici") +
  scale_fill_gradient(low = "green", high = "green") +
  theme_minimal()

# Plot top 10 unhappiest countries
ggplot(top_10_infelici, aes(x = reorder(`Country.name`, -`Ladder.score`), y = `Ladder.score`, fill = `Ladder.score`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`Ladder.score`, 2)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = "Country", y = "Ladder Score", title = "Top 10 Paesi più infelici") +
  scale_fill_gradient(low = "red", high = "red") +
  theme_minimal()


# come troviamo la posizione di un nostro paese a scelta?

trova_posizione_paese <- function(country_name, data) {
  posizione <- which(data$`Country.name` == country_name)
  return(posizione)
}

trova_paese <- "Italy"  

posizione_paese <- trova_posizione_paese(trova_paese, data)

print(posizione_paese)

# correlazioni
library(corrplot)

data_corelazioni <- data %>% 
  select(corruption = Perceptions.of.corruption,
         generosity = Generosity,
         freedom = Freedom.to.make.life.choices, 
         life_expectancy = Healthy.life.expectancy, 
         social_support = Social.support,
         GDP_per_capita = Logged.GDP.per.capita, 
         happiness = Ladder.score
  )

correlation_matrix <- cor(data_corelazioni)


corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7, addCoef.col = "black")


# top paesi più generosi e meno generosi (hanno a che fare con la felicità di un Paese??)

# Ordiniamo il datset per generosità
generosità_ordine <- data[order(-data$Generosity), ]

# Scegli quanti paesi vuoi visualizzare (top10)
top_generous <- head(generosità_ordine, 10)


ggplot(top_generous, aes(x = reorder(`Country.name`, Generosity), y = Generosity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Generosity, 2)), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +
  labs(x = "Paese", y = "Generosità", title = "Paesi più generosi") +
  coord_flip() +
  theme_minimal()

#stesso procedimento per i paesi meno generosi
bottom_generous <- tail(generosità_ordine, 10)


ggplot(bottom_generous, aes(x = reorder(`Country.name`, -Generosity), y = Generosity)) +
  geom_bar(stat = "identity", fill = "salmon") +
  geom_text(aes(label = round(Generosity, 2)), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +  
  labs(x = "Country", y = "Generosità", title = "Paesi meno generosi") +
  coord_flip() +  
  theme_minimal()

# top paesi per corruzione
#ordiniamo il dataset per corruzione





# upload altro dataset per analizzare la distribuzione tra gli anni 
data2 <- read.csv("C:/Users/ikry/Downloads/archive (5)/world-happiness-report.csv")
View(data2)


countries <- c("Italy", "France", "United States", "Finland", "Afghanistan", "Spain")

# Filter data for the selected countries
filtered_data <- data2[data2$`Country.name` %in% countries, ]

# Plot ladder scores across years for the selected countries
ggplot(filtered_data, aes(x = year, y = `Life.Ladder`, color = `Country.name`)) +
  geom_line() +
  labs(x = "Anno", y = "Score", color = "Paese") +
  theme_minimal()



