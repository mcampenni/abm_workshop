

library("igraph")

# THESE ARE GLOBAL VARIABLES

N <- 16
NGENERATIONS <- 10
NROUNDS <- 50

BEST.RATIO <- round(0.2 * N)
MU.RATE <- 0.05

REPETITIONS <- 1

# THIS IS THE DATA.FRAME WHERE WE STORE RESULTS
results <- data.frame(generation = 1:NGENERATIONS, 
                      median.strategy = 0, median.payoff = 0,
                      median.punishment = 0)

results.strategy.matrix <- array(0, c(NGENERATIONS, N))
results.payoff.matrix <- array(0, c(NGENERATIONS, N))
results.punishment.matrix <- array(0, c(NGENERATIONS, N))

# THESE ARE THE ELEMENTS FROM THE PAYOFF MATRIX

r.1 <- 1
r.2 <- 1

s.1 <- -1
s.2 <- 2

t.1 <- 2
t.2 <- -1

p.1 <- 0
p.2 <- 0

punish <- -0.5
punished <- -4

threshold <- 1.5

# R S
# T P

# this is the payoff matrix I am using here
# 1,1  -1,2
# 2,-1  0,0


# # THIS IS TO INITIALIZE THE POPULATION
# pop.init <- function(x){
#   individuals <- data.frame(id = 1:x, strategy = runif(x), payoff = 0, rank = 0, third.punish = runif(x), last.strategy = 0)
#   return(individuals)
# }
# 
# indDF <- pop.init(N)
# indDF.original <- indDF


library("igraph")
pop.init.net <- function(x){
  net <- graph.lattice(length = x, dim = 2)
  V(net)$id <- 1:(x * x)
  V(net)$strategy <- runif(x * x)
  V(net)$payoff <- 0
  V(net)$rank <- 0
  V(net)$third.punish <- runif(x * x)
  V(net)$last.strategy <- 0
  return(net)
}

indNET <- pop.init.net(round(sqrt(N)))
indNET.original <- indNET



# THIS IS TO ALLOW AGENTS TO PLAY THE GAME
# AND UPDATE THEIR PAYOFF
play.game <- function(ind, payoff.vector){
  
  if(ind$strategy[1] > runif(1)){
    ind$last.strategy[1] <- 1
  }else{
    ind$last.strategy[1] <- 2
  }
  
  if(ind$strategy[2] > runif(1)){
    ind$last.strategy[2] <- 1
  }else{
    ind$last.strategy[2] <- 2
  }
  
  if(ind$last.strategy[1] == 1 && ind$last.strategy[2] == 1){
    ind$payoff[1] <- ind$payoff[1] + payoff.vector[1]
    ind$payoff[2] <- ind$payoff[2] + payoff.vector[2]
  }
  
  if(ind$last.strategy[1] == 1 && ind$last.strategy[2] == 2){
    ind$payoff[1] <- ind$payoff[1] + payoff.vector[3]
    ind$payoff[2] <- ind$payoff[2] + payoff.vector[4]
  }
  
  if(ind$last.strategy[1] == 2 && ind$last.strategy[2] == 1){
    ind$payoff[1] <- ind$payoff[1] + payoff.vector[5]
    ind$payoff[2] <- ind$payoff[2] + payoff.vector[6]
  }
  
  if(ind$last.strategy[1] == 2 && ind$last.strategy[2] == 2){
    ind$payoff[1] <- ind$payoff[1] + payoff.vector[7]
    ind$payoff[2] <- ind$payoff[2] + payoff.vector[8]
  }
  
  return(ind)
}

# my
punishment <- function(ind, third, cost){
  if(third$third.punish > runif(1)){
    if(ind$last.strategy[1] == 2){
      ind$payoff[1] <- ind$payoff[1] + cost[2]
      third$payoff <- third$payoff + cost[1]
    }
    if(ind$last.strategy[2] == 2){
      ind$payoff[2] <- ind$payoff[2] + cost[2]
      third$payoff <- third$payoff + cost[1]
    }
  }
  return(list(ind, third))
}


# THIS IS TO IMPLEMENT THE REPRODUCTION PHASE
# ind -> the population
# ratio -> number of individuals selected for reproduction
# mu.rate -> mutation rate
# MY CODE
reproduction <- function(ind, ratio, mu.rate){
  
  ranking <- sort(ind$payoff, decreasing = TRUE)
  best.ids <- match(ranking, ind$payoff)
  
  best.ind <- ind[best.ids[1:ratio], ]
  best.ind.mu <- best.ind
  
  for(i in 1:dim(best.ind.mu)[1]){
    ifelse(mu.rate > runif(1), best.ind.mu$strategy[i] <- best.ind.mu$strategy[i] + (sign(rnorm(1000,0,0.5)[1]) * mu.rate),
           NA)
    ifelse(mu.rate > runif(1), best.ind.mu$third.punish[i] <- best.ind.mu$third.punish[i] + (sign(rnorm(1000,0,0.5)[1]) * mu.rate),
           NA)
  }
  
  ind[best.ids[(length(best.ids) - ratio + 1):length(best.ids)], ] <- best.ind.mu
  
  ind$id <- 1:dim(ind)[1]
  ifelse(ind$strategy < 0, ind$strategy <- 0, NA)
  ifelse(ind$strategy > 1, ind$strategy <- 1, NA)
  ifelse(ind$third.punish < 0, ind$third.punish <- 0, NA)
  ifelse(ind$third.punish > 1, ind$third.punish <- 1, NA)
  
  ind$rank <- 0
  ind$payoff <- 0
  ind$last.strategy <- 0
  
  return(ind)
}


# THIS IS A FUNCTION WE USE TO PARALLELIZE RUNS OF SAME SET OF PARAMETERS SIMULATIONS
multiResultClass <- function(result1 = NULL, result2 = NULL, 
                             result3 = NULL, result4 = NULL){
  me <- list(
    result1 = result1,
    result2 = result2,
    result3 = result3,
    result4 = result4
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"multiResultClass")
  return(me)
}



# # THIS IS MINE...
# # THIS IS THE ENGINE OF THE SIMULATION
# # i.e., THE MAIN METHOD

library("foreach")
library("doParallel")

#setup parallel backend to use 8 processors
cl<-makeCluster(detectCores())
registerDoParallel(cl)

start_time <- Sys.time()

ls <- foreach(icount(REPETITIONS), .combine = "rbind") %dopar% {
  
  for(i in 1:NGENERATIONS){
    print(paste("current generation is:", i, sep = " "))
    for(j in 1:NROUNDS){
      print(paste("current round is:", j, sep = " "))
      random.ids <- sample(N, N, replace = FALSE)
      
      library(igraph)
      for(k in 1:N){
        from <- k
        nei <- neighbors(indNET, k)
        to <- sample(nei)[1]
        fromto <- c(from,to)
        
        players <- data.frame(id = 1:2, 
                              strategy = 0, 
                              payoff = 0, 
                              rank = 0, 
                              third.punish = 0, 
                              last.strategy = 0)
        
        players$id <- V(indNET)$id[fromto]
        players$strategy <- V(indNET)$strategy[fromto]
        players$payoff <- V(indNET)$payoff[fromto]
        players$rank <- V(indNET)$rank[fromto]
        players$third.punish <- V(indNET)$third.punish[fromto]
        players$last.strategy <- V(indNET)$last.strategy[fromto]
        
        observer.id <- sample(random.ids[-match(c(from,to), random.ids)])[1]
        
        observer <- data.frame(id = 1, 
                              strategy = 0, 
                              payoff = 0, 
                              rank = 0, 
                              third.punish = 0, 
                              last.strategy = 0)
        
        observer$id <- V(indNET)$id[observer.id]
        observer$strategy <- V(indNET)$strategy[observer.id]
        observer$payoff <- V(indNET)$payoff[observer.id]
        observer$rank <- V(indNET)$rank[observer.id]
        observer$third.punish <- V(indNET)$third.punish[observer.id]
        observer$last.strategy <- V(indNET)$last.strategy[observer.id]
        
        
        players <- play.game(players, c(r.1, r.2, s.1, s.2, t.1, t.2, p.1, p.2))
        
        # if(median(indDF$last.strategy) > threshold){
          outcome <- punishment(players, observer, c(punish, punished))
          players <- outcome[[1]]
          observer <- outcome[[2]]
        # }
        
        V(indNET)$strategy[fromto] <- players$strategy 
        V(indNET)$payoff[fromto] <- players$payoff
        V(indNET)$rank[fromto] <- players$rank
        V(indNET)$third.punish[fromto] <- players$third.punish
        V(indNET)$last.strategy[fromto] <- players$last.strategy
        
        V(indNET)$strategy[observer.id] <- observer$strategy 
        V(indNET)$payoff[observer.id] <- observer$payoff
        V(indNET)$rank[observer.id] <- observer$rank
        V(indNET)$third.punish[observer.id] <- observer$third.punish
        V(indNET)$last.strategy[observer.id] <- observer$last.strategy

      }
    }
    
    results$median.strategy[i] <- median(V(indNET)$strategy)
    results$median.payoff[i] <- median(V(indNET)$payoff)
    results$median.punishment[i] <- median(V(indNET)$third.punish)
    
    results.strategy.matrix[i, ] <- V(indNET)$strategy
    results.payoff.matrix[i, ] <- V(indNET)$payoff
    results.punishment.matrix[i, ] <- V(indNET)$third.punish
    
    # indDF <- reproduction(indDF, BEST.RATIO, MU.RATE)
  }
  
  result <- multiResultClass()
  result$result1 <- results
  result$result2 <- results.strategy.matrix
  result$result3 <- results.payoff.matrix
  result$result4 <- results.punishment.matrix
  return(result)
}

end_time <- Sys.time()
stopCluster(cl)

print(paste("running time: ", end_time - start_time,sep=""))
print(paste("start time was: ", start_time,sep=""))
print(paste("end time was: ", end_time,sep=""))



# results <- ls[[1]]
# plot(1:NGENERATIONS, results$median.strategy, ylim = c(0,1),
#      main = "average strategy & punishment", xlab = NA, ylab = NA)
# points(1:NGENERATIONS, results$median.punishment, pch=20)
# title(xlab = "generations", ylab = "average strategy & punishment")



# data <- ls[1:REPETITIONS]

# par(mfrow=c(4,3))
# par(mar=c(2,2,2,2) + 0.1)

# for(i in 1:REPETITIONS){
#   results <- data[[i]]
#   plot(1:NGENERATIONS, results$median.strategy, ylim = c(0,1),
#        main = "average strategy & punishment", xlab = NA, ylab = NA)
#   points(1:NGENERATIONS, results$median.punishment, pch=20)
#   title(xlab = "generations", ylab = "average strategy & punishment")
# }



data.strategy <- array(0, c(REPETITIONS, NGENERATIONS))
data.punishment <- array(0, c(REPETITIONS, NGENERATIONS))

for(i in 1:REPETITIONS){
  temp <- ls[[i]]
  data.strategy[i, ] <- temp$median.strategy
  data.punishment[i, ] <- temp$median.punishment
}

boxplot(as.data.frame(data.strategy), ylim = c(0,1),
        main = "strategy distributions", xlab = NA, ylab = NA)
title(xlab = "generations", ylab = "strategy")

boxplot(as.data.frame(data.punishment), ylim = c(0,1),
        main = "punishment distributions", xlab = NA, ylab = NA)
title(xlab = "generations", ylab = "punishment")









hist(V(indNET)[strategy > 0.5]$payoff, xlim=c(-200,100))
hist(V(indNET)[strategy < 0.5]$payoff, xlim=c(-200,100))



