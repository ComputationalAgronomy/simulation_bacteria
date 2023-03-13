## calculate the distance of each microbe's around.
distance.table <- function(m,n){
  distance <- matrix(NA,length(m),length(m)-1) 
   for(i in 1:length(m)){
      distance[i,] <- sqrt((m[i]-m[-i])^2+(n[i]-n[-i])^2)
    }
    return(distance)
}

## The way to calculate the angle between two points.
angle.cal <- function(r,m){
  a <- r - m
  b <- c(1,0)
  if(m[2] >= r[2]){
    angle <- 2*pi-(acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)))))
  }else{
    angle <- acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)))) 
  }
  return(angle)
}

## calculate the angel of each individual to another.
angle.table <- function(x,y){
  loc <- cbind(x,y)
  dist <- matrix(NA,length(x),length(x)-1)
  j <- 1
  while(j <= length(x)){
    for(i in 1:(length(x)-1)){
      dist[j,i] <- angle.cal(loc[-j,][i,],loc[j,])
    }
    j <- j+1
  }
  return(dist)
}


general_movement <- function(n,t,boundary,radius,arc,ec){
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
    x <- x[inside]
    y <- y[inside]
    distance_tab <- distance.table(x,y)
    angle_tab <- angle.table(x,y)
    if(length(general_list) > 0){
      general_list <- lapply(seq_along(general_list), 
                                function(k){move(general_list[[k]],distance_tab[k,],
                                                 angle_tab[k,],radius,arc,ec)})
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
background_movement <- function(n,t,boundary,sd,radius,arc,ec,resource){
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
  angle_tab <- angle.table(x,y)
  distance_tab <- distance.table(x,y)
  background_list <- lapply(seq_along(background_list), 
                            function(k){move(background_list[[k]],distance_tab[k,],
                                             angle_tab[k,],sd,radius,arc,ec,resource)})
  
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
background_simulation <- function(n,t,boundary,sd,radius,arc,ec,resource,
                                  st_birth,birth_rate,st_death,death_rate){
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
      angle_tab <- angle.table(x,y)
      distance_tab <- distance.table(x,y)
      background_list <- lapply(seq_along(background_list), 
                                function(k){move(background_list[[k]],distance_tab[k,],
                                                 angle_tab[k,],sd,radius,arc,ec,resource)})
      
    }else{
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
## Birth
    background_list <- c(background_list,lapply(background_list,birth,st_birth,birth_rate))
    background_list <- background_list[!is.na(background_list)]
## Death 
    logic <- lapply(background_list,death,st_death,death_rate)
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

