## Grid research for movement 
resource_test <- function(n,t,boundary = 10,radius = 0,arc = 0,ec = 0,resource = c(5,5)){
  par(mfrow=c(2,3))
  g1 <- general_movement(n,t,boundary,sd = NA,radius,arc,ec)
  loc_print.microbe(g1, "No resource")
  g2 <- background_movement(n,t,boundary,sd = 2,radius,arc,ec,c(5,5))
  loc_print.microbe(g2, "sd = 2")
  g3 <- background_movement(n,t,boundary,sd = 1.5,radius,arc,ec,c(5,5))
  loc_print.microbe(g3, "sd = 1.5")
  g4 <- background_movement(n,t,boundary,sd = 1,radius,arc,ec,c(5,5))
  loc_print.microbe(g4, "sd = 1")
  g5 <- background_movement(n,t,boundary,sd = 0.5,radius,arc,ec,c(5,5))
  loc_print.microbe(g5, "sd = 0.5")
  g6 <- background_movement(n,t,boundary,sd = 0.1,radius,arc,ec,c(5,5))
  loc_print.microbe(g6, "sd = 0.1")
  par(mfrow=c(1,1))
}

## Check generations
check_movement <- function(n,t=100,boundary=10,sd=2,radius=0,arc=0,ec=0,resource=c(5,5)){
  par(mfrow=c(2,3))
  background_list <- lapply(1:n, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,-1,1),runif(1,-1,1)))
  })
  loc_print.microbe(background_list, "Age = 0") 
  iter <- 1
  while(iter<=20){
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
  loc_print.microbe(background_list, "Age = 20") 
  
  while(iter<=40){
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
  loc_print.microbe(background_list, "Age = 40") 
  
  while(iter<=60){
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
  loc_print.microbe(background_list, "Age = 60") 
  
  while(iter<=80){
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
  loc_print.microbe(background_list, "Age = 80") 
  
  
  while(iter<=100){
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
  loc_print.microbe(background_list, "Age = 100") 
  par(mfrow=c(1,1))
}


loc_printbig.microbe<- function(m){
  a <- b <- vector("numeric")
  for(i in 1:length(m)){
    a[i] <- c(m[[i]]$location[1])
    b[i] <- c(m[[i]]$location[2])
    c <- cbind(a,b)
  }
  plot(c,xlim = c(-15,15),ylim = c(-15,15),xlab="X",ylab="Y",cex=0.5,pch=18)
} 

## set speed to 1 
density_test_C1 <- function(n, t = 50, boundary = 20, sd = NA,radius=c(1,2),
                            arc=c(pi/36,pi/12,pi/4),ec = 1.1){
  par(mfrow=c(2,3))
  iter <- 1
  while(iter < 3){
  for(i in 1:3){
      loc_printbig.microbe(general_movement(n,t,boundary,sd,radius[iter],arc[i],ec))
  }
    iter <- iter+1
  }
  par(mfrow=c(1,1))
}




















