## calculate the density of each microbe's around.
density <- function(m,x,y,radius){
   n1 <- n2 <- n3 <- n4 <- distance <- vector("numeric")
   for(i in 1:length(m)){
      distance <- (x[i]-x[-i])^2+(y[i]-y[-i])^2
      n1[i] <- sum(x[-i] > x[i] & x[-i] < x[i]+radius & y[-i] > y[i] & y[-i] < y[i]+radius & distance<radius)
      n2[i] <- sum(x[-i] > x[i]-radius & x[-i] < x[i] & y[-i] > y[i] & y[-i] < y[i]+radius & distance<radius)
      n3[i] <- sum(x[-i] > x[i]-radius & x[-i] < x[i] & y[-i] > y[i]-radius & y[-i] < y[i] & distance<radius)
      n4[i] <- sum(x[-i] > x[i] & x[-i] < x[i]+radius & y[-i] > y[i]-radius & y[-i] < y[i] & distance<radius)
    }
    density_table <- cbind(1/(n1+1),1/(n2+1),1/(n3+1),1/(n4+1))
    return(density_table)
}

general_movement <- function(n,t,boundary,radius){
  general_list <- lapply(1:n, function(x){
    new_microbe(speed = runif(1,0,1), location=c(runif(1,0,2),runif(1,0,2)))
  })
  iter <- 1
  while(iter<=t){
    x <- y <- vector("numeric")
    for(i in 1:length(general_list)){
      x[i] <- general_list[[i]]$location[1]
      y[i] <- general_list[[i]]$location[2]
    }
    inside <- which(abs(x)<=boundary & abs(y)<=boundary)
    general_list <- general_list[inside] 
    if(length(general_list) > 0){
      microbe_density <- density(general_list,x,y,radius)
      general_list <- lapply(seq_along(general_list), 
                                function(k){move(general_list[[k]],microbe_density[k,])})
    }else{
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter+1
  }
  return(loc_print.microbe(general_list))
}

## Consider movement only first. 
## n:initial population, which will be the same through the simulation here.
## speed is set (0,1), radius is set (0,1)
## background_movement(100,100,10,1)
background_movement <- function(n,t,boundary,radius,resource){
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,-1,1),runif(1,-1,1)))
  })
iter <- 1
while(iter<=t){
  x <- y <- vector("numeric")
  for(i in 1:length(background_list)){
    x[i] <- background_list[[i]]$location[1]
    y[i] <- background_list[[i]]$location[2]
  }
  inside <- which(abs(x)<=boundary & abs(y)<=boundary)
  background_list <- background_list[inside]
  if(length(background_list) > 0){
  x <- x[inside]
  y <- y[inside]
  microbe_density <- density(background_list,x,y,radius)
  background_list <- lapply(seq_along(background_list), 
                            function(k){move(background_list[[k]],microbe_density[k,],resource)})
  
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
background_simulation <- function(n,t,boundary,radius,resource,
                                  st_birth,turning_point,birth_r1,birth_r2,st_death,death_r){
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,0,5),runif(1,0,5)))
  })
  iter <- 1
  while(iter <= t){
## Age
    background_list <- lapply(background_list,age)
## Move
    x <- y <- vector("numeric")
    for(i in 1:length(background_list)){
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
    inside <- which(abs(x)<=boundary & abs(y)<=boundary)
    background_list <- background_list[inside]
    if(length(background_list) > 0){
      x <- x[inside]
      y <- y[inside]
      microbe_density <- density(background_list,x,y,radius)
      background_list <- lapply(seq_along(background_list), 
                                function(k){move(background_list[[k]],microbe_density[k,],resource)})
      
    }else{
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
## Birth
    background_list <- c(background_list,lapply(background_list,birth,st_birth,turning_point,birth_r1,birth_r2))
    background_list <- background_list[!is.na(background_list)]
## Death 
    logic <- lapply(background_list,death,st_death,death_r)
    survive <- which(logic == "True")
    background_list <- background_list[survive]
    if(length(background_list)==0){
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }
    iter <- iter+1
  }
  return(loc_print.microbe(background_list))
  }
