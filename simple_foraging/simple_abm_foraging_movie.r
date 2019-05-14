
setwd("/Users/mc751/Desktop/simple_foraging/")

# GLOBAL VARIABLES

N <- 50
resources <- 20
environment.size <- 200
max.amount <- 3
steps <- 200

create.agents <- function(x){
  pop <- data.frame(id = rep("a", x), energy = 0, x = 0, y = 0, t = 0)
  for(i in 1:x){
    pop[i,]$x <- sample(seq(1,environment.size))[1]
    pop[i,]$y <- sample(seq(1,environment.size))[1]
  }
  return(pop)
}

create.resources <- function(x){
  resources <- data.frame(id = rep("r", x), energy = 0, x = 0, y = 0, t = 0)
  for(j in 1:x){
    resources[j,]$x <- sample(seq(1,environment.size))[1]
    resources[j,]$y <- sample(seq(1,environment.size))[1]
    resources[j,]$energy <- runif(1) * max.amount
  }
  return(resources)
}

eating <- function(agent, resources){
    for(res in 1:dim(resources)[1]){
      if(agent$x == resources[res,]$x && agent$y == resources[res,]$y){
        agent$energy <- agent$energy + resources[res,]$energy
        resources[res,]$energy <- 0
      } 
    }
  return(list(agent, resources))
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
    if(resources[target,]$energy > 0){
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



Pop<-create.agents(N)
Resources <- create.resources(resources)

results <- rbind(Pop, Resources)

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



# movements <- vector("list", steps)

for(t in 1:steps){
  print(t)
  
  for(a in sample(1:dim(Pop)[1])){
    outcome <- eating(Pop[a,], Resources)
    Pop[a,] <- outcome[[1]]
    Resources <- outcome[[2]]
  }

  # Pop <- look.for.resources.rw(Pop)
  Pop <- look.for.resources.clever(Pop, Resources)
  
  Pop$t <- Pop$t + 1
  Resources$t <- Resources$t + 1
  
  results <- rbind(results, Pop, Resources)
  
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



# for(t in 1:steps){
#   print(movements[[t]])
# }
# 
# 
# library(ggpubr)
# figure <- ggarrange(plotlist = movements,
#                     ncol = 4, nrow = 5)
# figure


library(gganimate)
library(gifski)
library(png)

anim <- ggplot(results, aes(x, y, color = id, size = results$energy + 1)) +
  geom_point() + 
  xlim(0,environment.size) + 
  ylim(0,environment.size) +
  # transition_time(t) +
  transition_states(t, transition_length = 1, state_length = 1) +
  shadow_wake(wake_length = 0.05) 
# +
#   ggtitle("{frame_time}") +
#   transition_time(t) +
#   ease_aes("linear") +
#   enter_fade() +
#   exit_fade()

# library(tweenr)
animate(anim)
anim_save(filename = "foraging_movie.gif")


# use "cmd + y" to play the .gif


