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


general_movement <- function(n,t,boundary,sd = NA,radius,arc,ec){
  general_list <- lapply(1:n, function(x){
    new_microbe(speed = 1, location=c(runif(1,-5,5),runif(1,-5,5)))
  })
  iter <- 1
  while(iter <= t){
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
                                                 angle_tab[k,],sd = NA,radius,arc,ec)})
    }else{
      print(paste("All microbes move out of the boundary at i th iteration where i = ", iter))
      break
    }
    iter <- iter+1
  }
  return(general_list)
}


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
  return(background_list)
}


loc.print <- function(background_list,tt){
  back <- list()
  general <- list()
  for(i in 1:length(background_list)){
    if(class(background_list[[i]])[2] == "background"){
      back <- c(back,list(background_list[[i]]))
    }else{
      general <- c(general,list(background_list[[i]]))
    }
  }
  background_list <- c(back,general)
  x <- y <- vector("numeric")
  for(i in 1:length(background_list)){
    x[i] <- background_list[[i]]$location[1]
    y[i] <- background_list[[i]]$location[2]
  }
  plot(x,y,xlim = c(-20,20),ylim = c(-20,20),col = c(rep("black",length(back)),rep("aquamarine4"
,length(general))),pch = 20,main = tt,cex = 0.5)
  points(5,5,col = "red",pch  = 19, cex = 1)
}






overall_simulation <- function(background_list,t,sd,radius=3,boundary = 20,arc=pi/6,ec = 2,resource = c(5,5),
                                  st_birth=1,birth_rate=0.5,st_death=1,death_rate=0.1,capacity=1000){
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
    pop <- length(background_list)
    background_list <- c(background_list,lapply(background_list,birth,st_birth,birth_rate,pop,capacity))
    background_list <- background_list[!is.na(background_list)]
## Death 
    pop <- length(background_list)
    logic <- lapply(background_list,death,st_death,death_rate,pop,capacity)
    survive <- which(logic == "True")
    background_list <- background_list[survive]
    if(length(background_list)==0){
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }
    iter <- iter+1
  }
  return(background_list)
  }

overall_search <- function(sd){
  par(mfrow=c(2,3))
  background_list <- lapply(1:100, function(x){
    background_microbe(speed = runif(1,0,1), location = c(runif(1,-2,2),runif(1,-2,2)))
  })
  general_list <- lapply(1:500, function(x){
    new_microbe(speed = runif(1,0,1), location = c(runif(1,-5,5),runif(1,-5,5)))
  })
  background_list <- c(background_list,general_list)
  loc.print(background_list, "Age = 0")
  
  background_list <- overall_simulation(background_list,20,sd)
  loc.print(background_list, "Age = 20")
  
  background_list <- overall_simulation(background_list,40,sd)
  loc.print(background_list, "Age = 40")
  
  background_list <- overall_simulation(background_list,60,sd)
  loc.print(background_list, "Age = 60")
  
  background_list <- overall_simulation(background_list,80,sd)
  loc.print(background_list, "Age = 80")
  
  background_list <- overall_simulation(background_list,100,sd)
  loc.print(background_list, "Age = 100")
  
  par(mfrow=c(1,1))
}






