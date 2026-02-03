# Cosmin Mitachi, Cerambyx cerdo 
# 18 novembre 2025

# Caricare i pacchetti 
library(primer) 
library(RColorBrewer) 
library(readr) 
library(deSolve) 
library(lattice)
library(nls2)
library(minpack.lm)

# Modello densità-dipendente ------------------------------------------------------------------------------------------------------------

# Definire i parametri
nyears = 100 # tempo per cui stimare l'andamento della popolazione
nreps = 1000 # numero di simulazioni da fare

# Parametri biologici della specie
l_max <- max(l) # calcolare valore massimo di lambda dai dati 
r_max = log(l_max) # massimo coefficiente di crescita
# r_max può essere calcolato o trovato in letteratura es: r_max = 1.36
N0 = N0 = mydata$count[length(mydata$count)] # abbondanza iniziale

## Calcolo capacità portante K ----------------------------------------------------------------------------------------------------------
# K può essere calcolata o travata in letteratura es: K = 1200

# Conversione dell'anno in tempo trascorso (t) 
mydata $time <- mydata $year - min(mydata$year)

# Adattamento del modello logistico usando nlsLM
logistic_model <- nlsLM(count ~ K/(1 + ((K - count[1])/count[1]) * exp(-r * time)), 
                        data = mydata, start = list(K = max(mydata$count), r = r_max))

# Estrazione dei parametri stimati 
estimated_parameters <- coef(logistic_model)

# Capacità portante (K) e tasso di crescita (r) 
K <- estimated_parameters["K"]
r <- estimated_parameters["r"]

# Stampa dei parametri stimati 
cat("Capacità portante (K):", K, "\n")
cat("Tasso di crescita (r):", r, "\n") 
round(K, 0)
round(r, 2)
# avete Trovato K e r

# Stocasticità ambientale
sd_lambda = sd # deviazione standard di l: l’avete calcolata prima 

# Densità-dipendenza (Ricker model)
Ricker = function(prev_abund){ # funzione per calcolare l'abbondanza degli anni futuri (include la stocasticità ambientale)
  prev_abund * exp(log(rlnorm(1, r_max, sd_lambda))*(1-(prev_abund/K)))
}

# Minacce (una o più di una, a seconda della specie)
# con più di una minaccia bisogna fare due PVA (però si può fare sinergia modificando lo script)
Fire_prob = 0.2 # 30% probabilità di incendi  
Fire_lambda = 0.5 # 50% della popolazione sopravvive all'incendio
#lambda 50 ci sta
#la probabilità incendi bisogna guardare zsc merlino perchè pva è fatta su quella
#la stima supportata da calcoli fornisce una probabilità realistica di 1,5%

## Creare la funzione per il modello ------------------------------------------------
PVAdemo = function(nreps, nyears, N0, r_max, K, Fire_prob, Fire_lambda){
  # browser()
  PopArray2 = array(0, dim = c((nyears + 1),nreps))
  # si inizia il loop con la funzione for per le repliche
  for(rep in 1:nreps){
    # si setta l'abbondanza iniziale
    PopArray2[1, rep] = N0
    # e poi il loop negli anni
    for(y in 2:(nyears + 1)){
      # stocasticità e d-d
      nextyear = max(0, trunc(Ricker(PopArray2[y- 1, rep])))
      # infine gli effetti delle minacce con la funzione if
      if(runif(1)<Fire_prob) nextyear = nextyear*Fire_lambda
      PopArray2[y, rep] = nextyear
    }
  }
  return(PopArray2)
}



## Far girare PVA --------------------------------------------------------------------------------------------------------------------------
Default = PVAdemo(nreps, nyears, N0, r_max, K, Fire_prob, Fire_lambda)

# Eliminare nan e inf 
Default[is.nan(Default)] = 0
Default[is.infinite(Default)] = 0

# Visualizzare la PVA 
PlotCloud = function(simdata){
  plot(c(1:101),simdata[,1],col=gray(0.7),type="l",ylim=c(0,max(simdata)),
       xlab="Years ",ylab="Abundance")
  for(r in 2:ncol(simdata)){ 
    lines(c(1:101),simdata[,r],col=gray(0.7),type="l")
  }
}

PlotCloud(Default)

# Visualizzare la PVA (log) 
PlotCloud(log(Default))

# Calcolare e visualizzare la probabilità di estinzione
Extinction_byyear = function(simdata){ apply(simdata,1,function(t)
  length(which(t==0)))/ncol(simdata)
}

plot(c(1:101),Extinction_byyear(Default),type="l",lwd=2,
     xlab="Year",ylab="Extinction risk")
abline(h=0.05,col="green",lwd=2) # aggiunge linea soglia probabilità di estinzione al 5%
abline(h=0.98,col="red",lwd=2) # aggiunge linea soglia probabilità di estinzione al 98%

# Visualizzare graficamente l’abbondanza finale 
hist(Default[nrow(Default),],xlab="Final abundance",ylab="Replicates",main="") 
abline(v=N0,col="green",lwd=2) # aggiunge linea dimensione iniziale della popolazione

# Calcolare la probabilità di estinzione (pop estinte/tot sim)
prob_decline = round(length(which(Default[nrow(Default),]<N0))/ncol(Default),2) 
cat("the probability of decline is: ", prob_decline)

# Valutare minacce crescenti 
Extinction_risk = function(simdata){ 
  length(which(simdata[nrow(simdata),]==0))/ncol(simdata)
}

Fire_lambdas = seq(0.9,0.1,by=-0.05) 
all_scenarios = numeric(length(Fire_lambdas)) 

for(scenario in 1:length(Fire_lambdas)){
  PVA = PVAdemo(nreps, nyears, N0, r_max, K, Fire_prob, Fire_lambdas[scenario]) 
  all_scenarios[scenario] = Extinction_risk(PVA)
}

plot(Fire_lambdas,all_scenarios,type="p",cex=2,
     xlab="Fire impacts (lambda)",ylab="Exctinction risk")
abline(h=0.05,col="red",lwd=2)


##EFFETTI POSITIVI

# Minacce (una o più di una, a seconda della specie)
# con più di una minaccia bisogna fare due PVA (però si può fare sinergia modificando lo script)
manage_prob = 0.3 # 30% probabilità di incendi  
manage_bonus = 0.2 # +10%

r_eff <- r_max
if(runif(1) < manage_prob){
  r_eff <- r_eff * 1.2
}

# Densità-dipendenza (Ricker model)
Ricker_manage = function(prev_abund, r_eff){ # funzione per calcolare l'abbondanza degli anni futuri (include la stocasticità ambientale)
  prev_abund * exp(log(rlnorm(1, r_eff, sd_lambda))*(1-(prev_abund/K)))
}

## Creare la funzione per il modello ------------------------------------------------
PVAdemo_manage = function(nreps, nyears, N0, r_max, K,
                   Fire_prob, Fire_lambda,
                   manage_prob, manage_bonus){
  
  PopArray2 = array(0, dim = c(nyears + 1, nreps))
  
  for(rep in 1:nreps){
    PopArray2[1, rep] = N0
    
    for(y in 2:(nyears + 1)){
      
      # r effettivo
      r_eff <- r_max
      if(runif(1) < manage_prob){
        r_eff <- r_eff * (1 + manage_bonus)
      }
      
      # crescita
      nextyear <- max(0, trunc(
        Ricker_manage(PopArray2[y-1, rep], r_eff)
      ))
      
      # incendio
      if(runif(1) < Fire_prob){
        nextyear <- nextyear * Fire_lambda
      }
      
      # vincolo ecologico
      nextyear <- min(nextyear, K)
      
      PopArray2[y, rep] <- nextyear
    }
  }
  return(PopArray2)
}




## Far girare PVA --------------------------------------------------------------------------------------------------------------------------
Default_manage = PVAdemo_manage(nreps, nyears, N0, r_eff, K, Fire_prob, Fire_lambda, manage_prob, manage_bonus)

# Eliminare nan e inf 
Default_manage[is.nan(Default_manage)] = 0
Default_manage[is.infinite(Default_manage)] = 0

# Visualizzare la PVA 
PlotCloud = function(simdata){
  plot(c(1:101),simdata[,1],col=gray(0.7),type="l",ylim=c(0,max(simdata)),
       xlab="Years ",ylab="Abundance")
  for(r in 2:ncol(simdata)){ 
    lines(c(1:101),simdata[,r],col=gray(0.7),type="l")
  }
}

PlotCloud(Default_manage)

# Visualizzare la PVA (log) 
PlotCloud(log(Default_manage))

# Calcolare e visualizzare la probabilità di estinzione
Extinction_byyear = function(simdata){ apply(simdata,1,function(t)
  length(which(t==0)))/ncol(simdata)
}

plot(c(1:101),Extinction_byyear(Default_manage),type="l",lwd=2,
     xlab="Year",ylab="Extinction risk")
abline(h=0.05,col="green",lwd=2) # aggiunge linea soglia probabilità di estinzione al 5%
abline(h=0.98,col="red",lwd=2) # aggiunge linea soglia probabilità di estinzione al 98%
