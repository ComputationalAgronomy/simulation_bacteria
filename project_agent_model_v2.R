n <- 1000 ## number of individuals
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

age_summary.agent <- function(m){
  a <- vector("numeric")
  for (i in 1:n){
   a[i] <- m[[i]]$age
  }
  return(hist(a,xlab = "age"))
} ## m here is all_microbe

loc_ind_summary.agent <- function(m){
  a <- b <- vector("numeric")
  for(i in 1:n){
    a[i] <- c(m[[i]]$loc[1,1])
    b[i] <- c(m[[i]]$loc[1,2])
    c <- cbind(a,b)
  }
  return(plot(c,main="all_microbe distribution",xlab="X",ylab="Y",cex=0.2,pch=18))
} ##m here is all_microbe

loc_time_summary.agent <- function(m,id){
  a <- b <- vector("numeric")
  for(i in 1:t){
    a[i] <- c(m[[i]][[id]]$loc[1,1])
    b[i] <- c(m[[i]][[id]]$loc[1,2])
    c <- cbind(a,b)
  } 
  return(plot(c,main="age-loc distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} ## m here is output  ##can't reflect time axis

move.agent <- function(m){
  p <- runif(n,0,1)
  theta <- vector("numeric",length = n)
  for(i in 1:n){
    if (p[i] < 1/2){
      theta[i] <- runif(1,0,pi/2)
    }else {
      theta[i] <- runif(1,pi/2,2*pi)
    }
  } ## prop of (0,90d) = 1/2, else =  2/3
  m$loc[1,1] <- m$loc[1,1]+m$speed*cos(theta[i])
  m$loc[1,2] <- m$loc[1,2]+m$speed*sin(theta[i])
  return(m)
}

age.agent <- function(m){
  m$age <- m$age + 1
  return(m)
}

death.agent <- function(m){
  m <- m[-i] ## m is all_microbe here
  return(m)
}

birth.agent <- function(m){
  m$loc[1,1] <- m$loc[1,1] + 1
  m$loc[1,2] <- m$loc[1,2] + 1
  return(m) ## m here is all_microbe[[i]]
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

summary(output[[1]][[1]]) ##first age, next id
loc_time_summary.agent(output,id = 1)
age_summary.agent(all_microbe)
loc_ind_summary.agent(all_microbe)

## birth of g2
all_microbe_g2 <- vector("list")
for (i in 1:n){
  all_microbe_g2[[i]] <- birth.agent(all_microbe[[i]])
}
## bind g1 and g2
all_microbe_1 <- rbind(all_microbe,all_microbe_g2)
summary.agent(all_microbe_1[[1000]])