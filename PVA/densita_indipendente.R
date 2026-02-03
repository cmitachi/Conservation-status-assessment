# Cosmin Mitachi, Cerambyx cerdo 
# 18 novembre 2025

# Pulire l’ambiente di lavoro
rm(list=ls())

#usate questa formula per installare tutti i pacchetti
install.packages(c("primer", "RColorBrewer", "readr", "deSolve", "lattice", "nls2", "minpack.lm"))

# Caricare i pacchetti 
library(primer) 
library(RColorBrewer) 
library(readr) 
library(deSolve) 
library(lattice)
library(nls2)
library(minpack.lm)

# Importare i dati
mydata <- read_delim("C:/Users/cosmin/Desktop/qgis/INVERTEBRATI/PVA/dataset/Cerambyx_cerdo.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(mydata)

# Visualizzare i dati
plot(count ~ year, data = mydata, type = "o", las = 1)


# Modello densità-indipendente ---------------------------------------------------------------------------------------------------------

# Calcolare lambda (l) per ogni anno
counts = mydata$count
l = counts[-1]/counts[-length(counts)]
round(l, 2) # approssimazione di l a due decimali

# Visualizzarne l’istogramma
hist(l, breaks = 20)

# Calcolare media e deviazione standard di l
mean <- mean(l)
sd <- sd (l)

# Proiettare l
set.seed(2) # questa funzione fa sì che gli elementi randomici introdotti vengano ripetuti allo stesso modo in ogni nuova run dello script
sim.l <- rnorm(100, mean = mean, sd = sd) 
sim.l <- round(sim.l, 2)

## Creare il modello - simulazione singola --------------------------------------------------------------------------------------------
time = 101 # tempo per cui stimare l'andamento della popolazione 
N0 = mydata$count[length(mydata$count)] # abbondanza iniziale 
N = vector(length = time) # vettore in cui immagazzinare Nt
N[1] = N0
sim.l = rnorm(time, mean = mean(l), sd = sd(l))
for (t in 2:time) {
  N[t] = N[t - 1] * sim.l[t - 1]
}

plot(1:(time), N, type = "o", las = 1, 
     xlab = "Year", ylab = "N")

## Creare il modello - 1000 simulazioni -----------------------------------------------------------------------------------------------
sims = 1000 # numero di simulazioni da fare
outmat = sapply(1:sims, function(x) {
  time = 101
  N0 = mydata$count[length(mydata$count)]
  N = vector(length = time)
  N[1] = N0
  sim.l = rnorm(time, mean = mean(l), sd = sd(l))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.l[t - 1]
  }
  N
})

matplot(1:time, outmat, type = "l", las = 1, lty = 5, 
        ylim = c(0, 4000), ylab = "N", xlab = "Year")

# Interpretare l’ouptut
dim(outmat)

# Calcolare la probabilità di estinzione
minpop = apply(outmat, 2, function(x) min(x) < 50)
ext.risk <- sum(minpop + 0)/sims*100 #proporzione di colonne con TRUE 
ext.risk

