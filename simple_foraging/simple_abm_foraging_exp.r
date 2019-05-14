

# GLOBAL VARIABLES

N <- 10
resources <- 10
environment.size <- 20
max.amount <- 3
steps <- 50

# strategy = 0 random-walk
# strategy = 1 min.distance
strategy <- 1
runs <- 10

create.agents <- function(x){
  pop <- data.frame(id = 1:x, energy = 0, x = 0, y = 0)
  for(i in 1:x){
    pop[i,]$x <- sample(seq(1,environment.size))[1]
    pop[i,]$y <- sample(seq(1,environment.size))[1]
  }
  return(pop)
}

create.resources <- function(x){
  resources <- data.frame(id = 1:x, amount = 0, x = 0, y = 0)
  for(j in 1:x){
    resources[j,]$x <- sample(seq(1,environment.size))[1]
    resources[j,]$y <- sample(seq(1,environment.size))[1]
    resources[j,]$amount <- runif(1) * max.amount
  }
  return(resources)
}

eating <- function(agent, resources, num.res){
    for(res in 1:dim(resources)[1]){
      if(agent$x == resources[res,]$x && agent$y == resources[res,]$y && resources[res,]$amount > 0){
        agent$energy <- agent$energy + resources[res,]$amount
        resources[res,]$amount <- 0
        num.res <- num.res + 1
      }
    }
  return(list(agent, resources, num.res))
}

look.for.resources.rw <- function(agents){
  for(a in sample(1:dim(agents)[1])){
    agents[a,]$x <- agents[a,]$x + sign(rnorm(1,0,1))
    agents[a,]$y <- agents[a,]$y + sign(rnorm(1,0,1))
    
    # THIS IS TO CHECK THAT THE AGENT DOES NOT CROSS THE BORDERS OF THE ENV
    if(agents[a,]$x > environment.size){
      agents[a,]$x <- environment.size
    }
    
    if(agents[a,]$y > environment.size){
      agents[a,]$y <- environment.size
    }
    
    if(agents[a,]$x < 0){
      agents[a,]$x <- 0
    }
    
    if(agents[a,]$y < 0){
      agents[a,]$y <- 0
    }
  }
  return(agents)
}

library(fields)
look.for.resources.clever <- function(agents, resources){
  for(a in sample(1:dim(agents)[1])){
    distances <- array(0, c(1,dim(resources)[1]))
    for(j in 1:dim(resources)[1]){
      distances[j] <- rdist(agents[a,3:4], resources[j,3:4])
    }
    
    target <- match(min(distances), distances)
    # IS THERE ANY FOOD LEFT THERE?
    if(resources[target,]$amount > 0){
      if(agents[a,]$x < resources[target,]$x){
        agents[a,]$x <- agents[a,]$x + 1
      }else{
        agents[a,]$x <- agents[a,]$x - 1
      }
      if(agents[a,]$y < resources[target,]$y){
        agents[a,]$y <- agents[a,]$y + 1
      }else{
        agents[a,]$y <- agents[a,]$y - 1
      }
    }else{
      agents[a,]$x <- agents[a,]$x + sign(rnorm(1,0,1))
      agents[a,]$y <- agents[a,]$y + sign(rnorm(1,0,1))
    }
    
    # THIS IS TO CHECK THAT THE AGENT DOES NOT CROSS THE BORDERS OF THE ENV
    if(agents[a,]$x > environment.size){
      agents[a,]$x <- environment.size
    }
    
    if(agents[a,]$y > environment.size){
      agents[a,]$y <- environment.size
    }
    
    if(agents[a,]$x < 0){
      agents[a,]$x <- 0
    }
    
    if(agents[a,]$y < 0){
      agents[a,]$y <- 0
    }
  }
  return(agents)
}



# Pop<-create.agents(10)
# plot(Pop$x, Pop$y, xlim=c(0, environment.size), ylim=c(0, environment.size))
#
# library(tidyverse)
# ggplot(Pop, aes(x, y)) +
#   geom_point() +
#   xlim(0,environment.size) +
#   ylim(0,environment.size)
#
# Resources <- create.resources(resources)
# plot(Resources$x, Resources$y, xlim=c(0, environment.size), ylim=c(0, environment.size),
#      pch = 20, cex = Resources$amount * 2, xlab = "x", ylab = "y")
#
# library(tidyverse)
# ggplot(Resources, aes(x, y)) +
#   geom_point(size = Resources$amount * 2) +
#   xlim(0,environment.size) +
#   ylim(0,environment.size)


# # PLOT INITIAL POSITIONS OF AGENTS
# library(tidyverse)
# ggplot(Pop, aes(x, y)) + 
#   geom_point(color = "blue") + 
#   xlim(0,environment.size) + 
#   ylim(0,environment.size) +
#   ggtitle("initial positions of agents")
# 
# # PLOT POSITIONS OF RESOURCES
# library(tidyverse)
# ggplot(Resources, aes(x, y)) + 
#   geom_point(size = Resources$amount * 2, color = "darkGreen") + 
#   xlim(0,environment.size) + 
#   ylim(0,environment.size) +
#   ggtitle("position of resources")


movements <- vector("list", steps)
consumed.resources <- array(0,c(runs,steps))

for(r in 1:runs){
  countr <- 0
  Pop<-create.agents(10)
  Resources <- create.resources(resources)
  for(t in 1:steps){
    print(paste(r,t,sep=" : "))
    
    for(a in sample(1:dim(Pop)[1])){
      outcome <- eating(Pop[a,], Resources, countr)
      Pop[a,] <- outcome[[1]]
      Resources <- outcome[[2]]
      countr <- outcome[[3]]
    }
    
    consumed.resources[r,t] <- countr
    
    if(strategy == 0){
      Pop <- look.for.resources.rw(Pop) 
    }
    if(strategy == 1){
      Pop <- look.for.resources.clever(Pop, Resources)
    }
    
    # # PLOT AFTER MOVING
    # library(tidyverse)
    # p <- ggplot(Pop, aes(x, y)) + 
    #   geom_point(color = "blue", size = Pop$energy + 1) + 
    #   xlim(0,environment.size) + 
    #   ylim(0,environment.size) +
    #   ggtitle(paste("position of agents at time t = ", t, sep=""))
    # 
    # p.both <- p + 
    #   geom_point(data = Resources, size = Resources$amount * 2, color = "darkGreen")
    # 
    # movements[[t]] <- p.both
  }
}



# for(t in 1:steps){
#   print(movements[[t]])
# }
# 
# 
# library(ggpubr)
# figure <- ggarrange(plotlist = movements,
#                     ncol = 4, nrow = 5)
# print(figure)


plot(1:steps, colMeans(consumed.resources), ylim=c(0,resources))
boxplot(consumed.resources, ylim=c(0,resources))

