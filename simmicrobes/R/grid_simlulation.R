## Consider movement only first. 
## n:initial population, which will be the same through the simulation here.
## speed is set (0,1), radius is set (0,1)
background_movement <- function(n,boundary,t){
  
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,0,5),runif(1,0,5)))
  })
iter <- 1
while(iter<=t){
  x <- y <- vector("numeric")
    for(i in 1:length(background_list)){
      x[i] <- background_list[[i]]$location[1]
      y[i] <- background_list[[i]]$location[2]
    }
  inside <- which(x[x<=boundary] & y[y<=boundary])
  
  if(length(inside)>0){
  x <- x[inside]
  y <- y[inside]
  inside_list <- background_list[inside]
  
  d <- vector("numeric")
    for(i in 1:length(inside_list)){
      d <- (x[i]-x[-i])^2+(y[i]-y[-i])^2
      n1 <- sum(x[-i] > x[i] & x[-i] < x[i]+1 & y[-i] > y[i] & y[-i] < y[i]+1 & d<1)
      n2 <- sum(x[-i] > x[i]-1 & x[-i] < x[i] & y[-i] > y[i] & y[-i] < y[i]+1 & d<1)
      n3 <- sum(x[-i] > x[i]-1 & x[-i] < x[i] & y[-i] > y[i]-1 & y[-i] < y[i] & d<1)
      n4 <- sum(x[-i] > x[i] & x[-i] < x[i]+1 & y[-i] > y[i]-1 & y[-i] < y[i] & d<1)
      inside_list[[i]] <- move.background(inside_list[[i]],n1,n2,n3,n4)
    }
  background_list <- c(inside_list,background_list[-inside])
  }else{
    print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
  }
  iter <- iter+1
}
  return(loc_print.microbe(background_list))
}
  



## Take birth and death into account
## k = limited population of each grid.
## t = iteration
## Not completed yet
background_simulation <- function(n,t,k,speed,boundary,st_birth,turning_point,b,c,st_death,d){
  grid <- matrix(c(0,0),boundary,boundary)
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = speed, location=c(runif(1,0.5,boundary+0.5),runif(1,0.5,boundary+0.5)))
  })
  
  for(i in 1:n){
    l1 <- round(background_list[[i]]$location)[1]
    l2 <- round(background_list[[i]]$location)[2]
    grid[l1,l2] <- grid[l1,l2]+1
  }
  
  iter <- 1
  
  while(iter <= t){
    o_loc1 <- o_loc2 <- loc1 <- loc2 <- vector("numeric")
    background_list <- lapply(background_list,age)
    
  for(i in 1:length(background_list)){
    o_loc1[i] <- background_list[[i]]$location[1]
    o_loc2[i] <- background_list[[i]]$location[2]
  }
## Move    
  background_list <- lapply(background_list, move)
  
  out <- vector("numeric")
  out_iter <- 1         
  for(i in 1:length(background_list)){
    l3 <- background_list[[i]]$location[1]
    l4 <- background_list[[i]]$location[2]
    if(!(l3>0.5&l3<boundary+0.5)|!(l4>0.5&l4<boundary+0.5)){
      out[out_iter] <- i
      out_iter <- out_iter+1
    }else{
      out_iter <- out_iter
    }
  }
  if(length(out)>0){
    for(i in 1:length(out)){
      o1 <- round(o_loc1[out[i]])
      o2 <- round(o_loc2[out[i]])
      grid[o1,o2] <- grid[o1,o2]-1
    }
    o_loc1 <- o_loc1[-out]
    o_loc2 <- o_loc2[-out]
    background_list <- background_list[-out]
  }
    
  for(i in 1:length(background_list)){
    loc1[i] <- round(background_list[[i]]$location)[1]
    loc2[i] <- round(background_list[[i]]$location)[2]
    if(grid[loc1[i],loc2[i]] >= k){ 
      background_list[[i]]$location[1] <- o_loc1[i]
      background_list[[i]]$location[2] <- o_loc2[i]    
    }else{
      grid[loc1[i],loc2[i]] <- grid[loc1[i],loc2[i]] + 1
      grid[round(o_loc1[i]),round(o_loc2[i])] <- grid[round(o_loc1[i]),round(o_loc2[i])] - 1
    }
  }
## Death 
    logic <- lapply(background_list,death,st_death,d)
    survive <- which(logic == "True")
    die <- which(logic == "False")
    death_list <- background_list[die]
    if(length(death_list)>0){
    for(i in 1:length(death_list)){
      lost1 <- round(death_list[[i]]$location)[1]
      lost2 <- round(death_list[[i]]$location)[2]
      grid[lost1,lost2] <- grid[lost1,lost2] - 1
    }
    }
    background_list <- background_list[survive]
    
## Birth
    birth_list <- lapply(background_list,birth,st_birth,turning_point,b,c)
    birth_list <- birth_list[!is.na(birth_list)]
    if(length(birth_list)>0){
    for(i in length(birth_list)){
      b1 <- round(birth_list[[i]]$location)[1]
      b2 <- round(birth_list[[i]]$location)[2]
      grid[b1,b2] <- grid[b1,b2] + 1
    }
    }
    background_list <- c(background_list,birth_list)
    
    l <- length(background_list)
    if(l == 0){
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }else{
      grid <- grid
    }
    iter <- iter+1
  }
  return(grid)
  }


