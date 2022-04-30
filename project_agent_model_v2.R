n <- 100 ## number of individuals
all_microbe <- vector("list",length = n)
for (i in 1:n){
  speed <- runif(n,1,5)
  all_microbe[[i]]<- list(id = i,age = 0,speed = speed[i],loc=matrix(c(0,0),1,2))
  class(all_microbe[[i]]) <- "agent"
}

summary.agent <- function(m){
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\n", m$id, m$age, m$speed, m$loc[1], m$loc[2])
  return(cat(output))
}

move.agent <- function(m){
  theta <- runif(n,0,2*pi)
  m$loc[1,1] <- m$loc[1,1]+m$speed*cos(theta[i])
  m$loc[1,2] <- m$loc[1,2]+m$speed*sin(theta[i])
  return(m)
}

age.agent <- function(m){
  m$age <- m$age + 1
  return(m)
}

t <- 50 ## number of iterations
iter <- 0
output <- vector("list",length = t)
while(iter < t){
for (i in 1:n){
  all_microbe[[i]] <- age.agent(all_microbe[[i]])
  all_microbe[[i]] <- move.agent(all_microbe[[i]])
}
  output[[iter+1]] <- all_microbe
  iter <- iter+1
}

summary(output[[1]][[1]])
##"agent" makes summary turn out "Null"(can't read the all_microbe but the individual)
## far more difficult to read than a tibble
