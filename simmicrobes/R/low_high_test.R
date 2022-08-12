## This file is for test.
## Fix the parameters of birth and death rate function, basically, nine sets are used: low, medium, high birth rate
## and low, medium,high death rate.
## Initial population and iterations are fixed. 
## Ignore movement here.
## Ignore parents here
low_low_test <-  function(n,t){
  microbe_list <- lapply(1:n, function(x){
    new_microbe(speed = runif(1,1,5), location=c(0,0))
  })
  id <- old <- amount <- average_age <- vector("numeric")
  microbe_table <- vector("list")
  iter <- 1
  while(iter <= t){
  microbe_list <- lapply(microbe_list,age)
  microbe_list <- c(microbe_list,lapply(microbe_list,birth))
  microbe_list <- microbe_list[!is.na(microbe_list)]
  logic <- lapply(microbe_list, death)
  survive <- which(logic == "True")
  microbe_list <- microbe_list[survive]
   for(i in 1:length(microbe_list)){
    id[i]<- microbe_list[[i]]$id
    old[i] <- microbe_list[[i]]$age
    microbe_table[[iter]] <- data.frame(id,old)
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
