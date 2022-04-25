library(tidyverse)
n <- 100
day <- 0
agent_1 <- tibble(id=c(1:n),day,speed=1,loc=matrix(c(0,0),1,2))
attach(agent_1)

move_1 <- function(n,i,speed,loc){
  theta <- runif(n,0,2*pi)
  loc[i,1] <- loc[i,1]+speed[i]*cos(theta[i])
  loc[i,2] <- loc[i,2]+speed[i]*sin(theta[i])
  return(loc)
}

output_1 <- vector("list")
while(day<51){
  for(i in 1:n){
    loc <- move_1(n,i,speed,loc)
  }
  agent_1 <- tibble(id=c(1:n),day,speed=1,loc)
  output_1[day+1] <- list(agent_1)
  day <- day+1
}

propagate <-cbind(output_1[[51]]$loc[,1]+1,output_1[[51]]$loc[,2]-1)
agent_2 <- rbind(agent_1,tibble(id=c((n+1):(2*n)),day=day-1,speed=1,loc=propagate))
