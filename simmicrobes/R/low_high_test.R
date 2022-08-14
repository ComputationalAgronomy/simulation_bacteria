## This file is for test.
## Fix the parameters of birth and death rate function, basically, nine sets are used: low, medium, high birth rate
## and low, medium,high death rate.
## Initial population and iterations are fixed. 
## Ignore movement here.

microbe_test <-  function(n,t,p,st_birth,mean,sd,st_death,b){
  microbe_list <- lapply(1:n, function(x){
    new_microbe(speed = runif(1,1,5), location=c(0,0))
  })
  id <- old <- amount <- average_age <- vector("numeric")
  microbe_table <- vector("list")
  iter <- 1
  while(iter <= t){
  microbe_list <- lapply(microbe_list,age)
  logic <- lapply(microbe_list, death,st_death,b)
  survive <- which(logic == "True")
  microbe_list <- microbe_list[survive]
  microbe_list <- c(microbe_list,lapply(microbe_list,birth,p,st_birth,mean,sd))
  microbe_list <- microbe_list[!is.na(microbe_list)]
  id <- old <- amount <- average_age <- vector("numeric")
  l <- length(microbe_list)
  
  if(l == 0){
    print(paste("All microbes die at i th iteration where i = ", iter))
    break
  }else{
    for(i in 1:l){
      id[i]<- microbe_list[[i]]$id
      old[i] <- microbe_list[[i]]$age
      microbe_table[[iter]] <- data.frame(id,old)
    }
  }
  iter <- iter+1
  }
  for(i in 1:length(microbe_table)){
    amount[i] <- nrow(microbe_table[[i]])
    average_age[i] <- colMeans(microbe_table[[i]][2])
    table <- data.frame(amount,average_age)
  }
  return(table)
}
