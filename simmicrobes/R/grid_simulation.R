## Consider movement only first. 
## n:initial population, which will be the same through the simulation here.
## speed is set (0,1), radius is set (0,1)
background_movement <- function(n,boundary,t){
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,0,5),runif(1,0,5)))
  })
iter <- 1
while(iter<=t){
  x <- y <- n1 <- n2 <- n3 <- n4 <- vector("numeric")
    for(i in 1:length(background_list)){
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
  inside <- which(abs(x)<=boundary & abs(y)<=boundary)
  background_list <- background_list[inside]
  
  if(length(background_list)>0){
  x <- x[inside]
  y <- y[inside]
  
  distance <- vector("numeric")
    for(i in 1:length(background_list)){
      distance <- (x[i]-x[-i])^2+(y[i]-y[-i])^2
      n1[i] <- sum(x[-i] > x[i] & x[-i] < x[i]+1 & y[-i] > y[i] & y[-i] < y[i]+1 & d<1)
      n2[i] <- sum(x[-i] > x[i]-1 & x[-i] < x[i] & y[-i] > y[i] & y[-i] < y[i]+1 & d<1)
      n3[i] <- sum(x[-i] > x[i]-1 & x[-i] < x[i] & y[-i] > y[i]-1 & y[-i] < y[i] & d<1)
      n4[i] <- sum(x[-i] > x[i] & x[-i] < x[i]+1 & y[-i] > y[i]-1 & y[-i] < y[i] & d<1)
    }
  density <- cbind(n1,n2,n3,n4)
  background_list <- lapply(seq_along(background_list), function(i){move(background_list[[i]],density[i,])})
  }else{
    print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
    break
  }
  iter <- iter+1
}
  return(loc_print.microbe(background_list))
}
  

## Take age,birth and death into account
## n = initial population
## t = iteration 
## boundary,once a microbe moves out of the boundary,it will die.
## st_birth,turning point,b,c are parameters set for birth,which have been introduced in birth function
## st_death,d are parameters set for death.
background_simulation <- function(n,t,boundary,st_birth,turning_point,b,c,st_death,d){
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,0,5),runif(1,0,5)))
  })
  iter <- 1
  while(iter <= t){
## Age
    background_list <- lapply(background_list,age)
## Move
    x <- y <- n1 <- n2 <- n3 <- n4 <- vector("numeric")
    for(i in 1:length(background_list)){
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x)<=boundary & abs(y)<=boundary)
    if(length(inside)>0){
      x <- x[inside]
      y <- y[inside]
      background_list <- background_list[inside]
      
      distance <- vector("numeric")
      for(i in 1:length(background_list)){
        distance <- (x[i]-x[-i])^2+(y[i]-y[-i])^2
        n1[i] <- sum(x[-i] > x[i] & x[-i] < x[i]+1 & y[-i] > y[i] & y[-i] < y[i]+1 & d<1)
        n2[i] <- sum(x[-i] > x[i]-1 & x[-i] < x[i] & y[-i] > y[i] & y[-i] < y[i]+1 & d<1)
        n3[i] <- sum(x[-i] > x[i]-1 & x[-i] < x[i] & y[-i] > y[i]-1 & y[-i] < y[i] & d<1)
        n4[i] <- sum(x[-i] > x[i] & x[-i] < x[i]+1 & y[-i] > y[i]-1 & y[-i] < y[i] & d<1)
      }
      density <- cbind(n1,n2,n3,n4)
      background_list <- lapply(seq_along(background_list), function(i){move.background(background_list[[i]],density[i,])})
    }else{
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
## Death 
    logic <- lapply(background_list,death,st_death,d)
    survive <- which(logic == "True")
    background_list <- background_list[survive]
    if(length(background_list)==0){
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }
## Birth
    background_list <- c(background_list,lapply(background_list,birth,st_birth,turning_point,b,c))
    background_list <- background_list[!is.na(background_list)]
    iter <- iter+1
  }
  return(loc_print.microbe(background_list))
  }

