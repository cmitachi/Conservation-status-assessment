Def_prob=0.15
Def_lambda=0.3
Syn_factor=0.3
#Syn_factor < 1 
#Riduce la popolazione aggiuntiva rispetto agli impatti dei singoli eventi. 
#Intensifica la mortalità.

#Syn_factor = 1
#Nessuna sinergia. Gli impatti restano solo additivi.

#Syn_factor > 1
#Aumenta la sopravvivenza quando i due eventi coincidono. 
#Questo è ecologicamente anomalo perché trasformerebbe la sinergia in un effetto compensatorio.

N0=70000

PVAdemo2 = function(nreps, nyears, N0, r_max, K,
                    Fire_prob, Fire_lambda,
                    Def_prob, Def_lambda,
                    Syn_factor){
  
  PopArray2 = array(0, dim = c(nyears + 1, nreps))
  
  for(rep in 1:nreps){
    PopArray2[1, rep] = N0
    
    for(y in 2:(nyears + 1)){
      
      # crescita con Ricker e densità-dipendenza
      nextyear = max(0, trunc(Ricker(PopArray2[y-1, rep])))
      
      # eventi stocastici
      fire_event = runif(1) < Fire_prob
      def_event  = runif(1) < Def_prob
      
      # applicazione effetti
      if(fire_event) nextyear = nextyear * Fire_lambda
      if(def_event)  nextyear = nextyear * Def_lambda
      
      # sinergia se entrambi gli eventi avvengono nello stesso anno
      if(fire_event && def_event) nextyear = nextyear * Syn_factor
      
      PopArray2[y, rep] = nextyear
    }
  }
  
  return(PopArray2)
}


Sim2 = PVAdemo2(nreps, nyears, N0, r_max, K,
                Fire_prob, Fire_lambda,
                Def_prob, Def_lambda,
                Syn_factor)

## Mortalità indotta dalle minacce ---------------------------------------------

ThreatMortality = function(simdata, N0,
                           Fire_prob, Fire_lambda,
                           Def_prob, Def_lambda,
                           Syn_factor){
  
  years = nrow(simdata) - 1
  
  fire_loss = numeric(years)
  def_loss  = numeric(years)
  syn_loss  = numeric(years)
  
  for(y in 2:nrow(simdata)){
    N_prev = simdata[y-1,]
    N_next = simdata[y,]
    
    # stima perdita per incendio
    fire_loss[y-1] = mean(N_prev * (1 - Fire_lambda)) * Fire_prob
    
    # stima perdita per deforestazione
    def_loss[y-1]  = mean(N_prev * (1 - Def_lambda)) * Def_prob
    
    # stima perdita sinergica
    syn_loss[y-1]  = mean(N_prev * (1 - Syn_factor)) * (Fire_prob * Def_prob)
  }
  
  return(list(fire=fire_loss, def=def_loss, syn=syn_loss))
}

M = ThreatMortality(Sim2, N0,
                    Fire_prob, Fire_lambda,
                    Def_prob, Def_lambda,
                    Syn_factor)

## Grafico comparativo della mortalità -----------------------------------------

plot(1:length(M$fire), M$fire, type="l", lwd=2,
     xlab="Year", ylab="Mortality", col="red")
lines(1:length(M$def), M$def, lwd=2, col="blue")
lines(1:length(M$syn), M$syn, lwd=2, col="black")
par(mar = c(3, 3, 3, 10), xpd = TRUE)
legend("topright", inset = c(-0.5, 0.1), legend=c("Fire", "Deforestation", "Synergy"),
       col=c("red","blue","black"),
       lwd=2)






PVAdemo_synergy <- function(nreps, nyears, N0,
                            r_max, K,
                            Fire_prob, Fire_lambda,
                            Def_prob, Def_lambda,
                            Syn_factor){
  
  Pop <- matrix(0, nyears+1, nreps)
  Pop[1, ] <- N0
  
  # matrici per le perdite
  Loss_fire <- Loss_def <- Loss_syn <- matrix(0, nyears+1, nreps)
  
  for(rep in 1:nreps){
    for(y in 2:(nyears+1)){
      # crescita
      N_prev <- Pop[y-1, rep]
      N_next <- max(0, trunc(N_prev * exp(r_max * (1 - N_prev / K))))
      
      # eventi
      fire_event <- runif(1) < Fire_prob
      def_event  <- runif(1) < Def_prob
      
      # perdita attesa dai singoli eventi
      loss_f <- if(fire_event) N_next * (1 - Fire_lambda) else 0
      loss_d <- if(def_event)  N_next * (1 - Def_lambda)  else 0
      
      # popolazione dopo singoli effetti
      N_after_single = N_next - loss_f - loss_d
      
      # perdita aggiuntiva dovuta alla sinergia
      loss_s <- if(fire_event && def_event) N_after_single * (1 - Syn_factor) else 0
      
      # popolazione finale
      N_post <- trunc(N_after_single - loss_s)
      N_post <- max(N_post, 0)
      
      # salva valori
      Pop[y, rep]       <- N_post
      Loss_fire[y, rep] <- loss_f
      Loss_def[y, rep]  <- loss_d
      Loss_syn[y, rep]  <- loss_s
    }
  }
  
  return(list(Pop=Pop,
              Loss_fire=Loss_fire,
              Loss_def=Loss_def,
              Loss_syn=Loss_syn))
}

# funzione per calcolare e plottare tassi osservati
PlotMortalityRates <- function(sim_out){
  nyears <- nrow(sim_out$Pop) - 1
  nreps  <- ncol(sim_out$Pop)
  fire_rate <- def_rate <- syn_rate <- numeric(nyears)
  
  for(y in 2:(nyears+1)){
    N_before <- sim_out$Pop[y-1, ]
    valid <- N_before > 0
    if(any(valid)){
      fire_rate[y-1] <- mean(sim_out$Loss_fire[y, valid]/N_before[valid])
      def_rate[y-1]  <- mean(sim_out$Loss_def[y, valid]/N_before[valid])
      syn_rate[y-1]  <- fire_rate[y-1] + def_rate[y-1]
    }
  }
  
  years <- 1:nyears
  par(mar = c(3, 3, 3, 10), xpd = TRUE)
  plot(years, fire_rate, type="l", lwd=2, col="red",
       ylim=c(min(c(fire_rate, def_rate, syn_rate)),max(c(fire_rate, def_rate, syn_rate))),
       xlab="Year", ylab="Observed mortality rate")
  lines(years, def_rate, lwd=2, col="blue")
  lines(years, syn_rate, lwd=2, col="black")
  legend("topright", inset = c(-0.6, 0.2), legend=c("Fire","Deforestation","Synergy"),
         col=c("red","blue","black"), lwd=2)
}

# --- Esempio di esecuzione ---
nreps <- 1000
nyears <- 100
N0 <- 583
r_max <- 0.1
K <- 630
Fire_prob <- 0.015
Fire_lambda <- 0.5
Def_prob <- 0.02
Def_lambda <- 0.5
Syn_factor <- 0.3  # maggiore effetto sinergico

sim <- PVAdemo_synergy(nreps, nyears, N0, r_max, K,
                       Fire_prob, Fire_lambda,
                       Def_prob, Def_lambda,
                       Syn_factor)

PlotMortalityRates(sim)


# QUANTI INDIVIDUI N0 HO BISOGNO PER AVERE UNA SOPRAVVIVENZA DI X?
PVAdemo_func <- function(mid,
                         Fire_prob,
                         Fire_lambda,
                         Def_prob,
                         Def_lambda,
                         r_max,
                         K) {
  sim <- PVAdemo_synergy(nreps, nyears, N0, r_max, K,
                         Fire_prob, Fire_lambda,
                         Def_prob, Def_lambda,
                         Syn_factor)
  # calcola probabilità di sopravvivenza dopo nyears
  final_pop <- sim$Pop[nyears+1, ]
  prob_survive <- mean(final_pop > 0)
  return(prob_survive)
}

# Funzione per trovare N0 minimo
FindN0 <- function(target_years, target_prob,
                   max_N0 = 10000, tol = 0.01,
                   Fire_prob, Fire_lambda,
                   Def_prob, Def_lambda,
                   r_max, K){
  
  nyears <<- target_years  # globale per PVAdemo_func
  low <- 1
  high <- max_N0
  N0_best <- NA
  
  while(low <= high){
    mid <- floor((low + high)/2)
    prob <- PVAdemo_func(mid,
                         Fire_prob,
                         Fire_lambda,
                         Def_prob,
                         Def_lambda,
                         r_max,
                         K)
    if(abs(prob - target_prob) < tol){
      N0_best <- mid
      break
    } else if(prob < target_prob){
      low <- mid + 1
    } else {
      high <- mid - 1
      N0_best <- mid
    }
  }
  return(N0_best)
}

# --- Esempio ---
target_years <- 100
target_prob  <- 0.3  # voglio 95% di sopravvivenza
Fire_prob <- 0.3
Fire_lambda <- 0.5
Def_prob <- 0.2
Def_lambda <- 0.4
r_max <- 0.1
K <- 700

N0_min <- FindN0(target_years, target_prob,
                 Fire_prob=Fire_prob, Fire_lambda=Fire_lambda,
                 Def_prob=Def_prob, Def_lambda=Def_lambda,
                 r_max=r_max, K=K)

cat("Numero minimo di individui iniziali per persistere", target_years,
    "anni con probabilità", target_prob, ":", N0_min, "\n")

