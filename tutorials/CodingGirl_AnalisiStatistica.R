setwd("") # indicare nelle virgolette il path dove avete scaricato i dati 
library(readxl)
dati <- read_excel("Istruzione.xlsx")

#### Descrittive

# numeriche 
d=dim(dati)
apply(dati[,-c(1,2,d[2])], 2, summary)

# categoriche 
table(dati$Area)
# posso cambiare l'ordine delle cateoorie che R mette in ordine alfabetico
dati$Area <- factor(dati$Area, levels=c('Nord-Ovest','Nord-Est','Centro', 'Sud-Isole'))
table(dati$Area)
barplot(table(dati$Area), col=c("green","blue","pink","red"))

## analizziamo, per esempio, i NEET 
y=dati$`Giovani che non lavorano e non studiano (NEET)`
hist(y, main="Giovani che non lavorano e non studiano (NEET)", col="yellow")
boxplot(y, main="Giovani che non lavorano e non studiano (NEET)", col = "orange")
# Intervallo di confidenza al 95% per la media 
t.test(y, conf.level = 0.95)$conf.int

################################################################################
# Domanda 1: L'uscita precoce del sistema di istruzione e formazione 
# (in alcune regioni è oltre il 15%) dipende dalla zona? 
################################################################################
y=dati$`Uscita precoce dal sistema di istruzione e formazione`
summary(y)
hist(y)
# creiamo una variabile dicotomica (dummy): 0-inferiore il 15%, 1-superiore il 15%
dati$dropout <- ifelse(y>15, 1,0)
dati$dropout <- factor(dati$dropout, levels = c(0,1), labels = c("No", "Si"))
table(dati$dropout)

# analizziamo ora l'associazione tra Dropout (dicotomica) e Zona 
y=dati$dropout
x=dati$Area
mytable <- table(y,x)
mytable 
plot(mytable, col=c("green","blue","pink","red"))
plot(t(mytable), col=c("pink","red"))
margin.table(mytable, 1)  
margin.table(mytable, 2) 
# Percentuali totali, per riga e per colonna #
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages
Test <- chisq.test(mytable, correct=FALSE)
Test

################################################################################
# Domanda 2: Il numero di NEET dipende dal Dropout?  ###########################
################################################################################
x=dati$dropout
y=dati$`Giovani che non lavorano e non studiano (NEET)`
boxplot(y ~ x, xlab = "Zone", ylab="NEET", col="orange")
library(gplots)
plotmeans(y ~ x, xlab = "Zone", ylab="NEET")
library(plyr)
D=data.frame(y,x)
ddply(D,~x,summarise,mean=mean(y),sd=sd(y),n=length(y))
t.test(y~x, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=dati)

################################################################################
# Domanda 3: Il numero di NEET varia in modo significativo nelle zone? #########
################################################################################
y=dati$`Giovani che non lavorano e non studiano (NEET)`
x=dati$Area
boxplot(y ~ x, xlab = "Zone", ylab="NEET", col="orange")
library(gplots)
plotmeans(y ~ x, xlab = "Zone", ylab="NEET")
library(plyr)
D=data.frame(y,x)
ddply(D,~x,summarise,mean=mean(y),sd=sd(y),n=length(y))
summary(aov(y ~ x, data=D))

################################################################################
# Domanda 4: Il numero Laureati e altri titoli terziari dipende da: 
# - Partecipazione culturale fuori casa
# - Lettura di libri e quotidiani
# - Fruizione delle biblioteche
################################################################################

y=dati$`Laureati e altri titoli terziari (30-34 anni)`
x1=dati$`Partecipazione culturale fuori casa`
x2=dati$`Lettura di libri e quotidiani`
x3=dati$`Fruizione delle biblioteche`
D=data.frame(y,x1,x2,x3)
C=cor(D)
pairs(~y+x1+x2+x3,data=D)
library(corrplot)
corrplot(C, method = 'number') 
corrplot(C, method = 'color', order = 'alphabet')
corrplot(C) # by default, method = 'circle'

## Regressione lineare semplice
# Stimotori dei minimi quadrati ordinari
mod1<-lm(y~x1, data=D)
summary(mod1)
mod2<-lm(y~x2, data=D)
summary(mod2)
mod3<-lm(y~x3, data=D)
summary(mod3)

## Regressione lineare multipla
mod<-lm(y~x1+x2+x3, data=D)
summary(mod)
# controllo collinarità 
library(car)
vif(mod)

# Scatter Plot
par(mfcol = c(1, 3))
plot(x1, y, xlab = "Partecipazione culturale fuori casa", 
     main = "Laureati e altri titoli terziari", pch = 15)
# aggiungi la retta interpolante
abline(lm(y~x1), col = "red") 

plot(x2, y, xlab = "Lettura di libri e quotidiani", 
     main = "Laureati e altri titoli terziari", pch = 15)
# aggiungi la retta interpolante
abline(lm(y~x2), col = "red") 

plot(x3, y, xlab = "Fruizione delle biblioteche", 
     main = "Laureati e altri titoli terziari", pch = 15)
# aggiungi la retta interpolante
abline(lm(y~x3), col = "red") 

