## This file is for test.
## Fix the parameters of birth and death rate function, basically, nine sets are used: low, medium, high birth rate
## and low, medium,high death rate.
## Initial population and iterations are fixed. 
## Ignore movement here.

microbe_test <-  function(n,t,st_birth,turning_point,b,c,st_death,d){
  microbe_list <- lapply(1:n, function(x){
    new_microbe(speed = runif(1,1,5), location=c(0,0))
  })
  id <- old <- amount <- average_age <- vector("numeric")
  microbe_table <- vector("list")
  iter <- 1
  while(iter <= t){
  microbe_list <- lapply(microbe_list,age)
  microbe_list <- c(microbe_list,lapply(microbe_list,birth,st_birth,turning_point,b,c))
  microbe_list <- microbe_list[!is.na(microbe_list)]
  logic <- lapply(microbe_list,death,st_death,d)
  survive <- which(logic == "True")
  microbe_list <- microbe_list[survive]
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


## n = initial population 
## t = iteration(age)
## r = replication
amount_test <- function(n,t,r){
par(mfrow=c(3,3))
low_low_test <- vector("list")
low_low_amount <- low_low_age <- matrix(0,t,r)
for(i in 1:r){
  low_low_test[[i]] <- microbe_test(n,t,10,30,0.5,2,10,0.5)
  low_low_amount[,i] <- low_low_test[[i]][,1]
}
low_low_mean_amount <- apply(low_low_amount,1,mean)
plot(1:t,low_low_mean_amount,type = 'l',lwd=2,ylim = c((1/2)*(min(low_low_mean_amount)),max(low_low_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'low_low_test')

low_median_test <- vector("list")
low_median_amount <- matrix(0,t,r)
for(i in 1:r){
  low_median_test[[i]] <- microbe_test(n,t,10,30,0.5,2,10,1)
  for(j in 1:length(low_median_test[[i]][,1])){
    low_median_amount[j,i] <- low_median_test[[i]][,1][j]
  }
}
low_median_mean_amount <- apply(low_median_amount,1,mean)
plot(1:t,low_median_mean_amount,type = 'l',lwd=2,ylim = c(0,max(low_median_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'low_median_test')

low_high_test <- vector("list")
low_high_amount <- matrix(0,t,r)
for(i in 1:r){
  low_high_test[[i]] <- microbe_test(n,t,10,30,0.5,2,10,2)
  for(j in 1:length(low_high_test[[i]][,1])){
    low_high_amount[j,i] <- low_high_test[[i]][,1][j]
  }
}
low_high_mean_amount <- apply(low_high_amount,1,mean)
plot(1:t,low_high_mean_amount,type = 'l',lwd=2,ylim = c(0,max(low_high_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'low_high_test')

median_low_test <- vector("list")
median_low_amount <- matrix(0,t,r)
for(i in 1:r){
  median_low_test[[i]] <- microbe_test(n,t,10,30,1,1,10,0.5)
  median_low_amount[,i] <- median_low_test[[i]][,1]
}
median_low_mean_amount <- apply(median_low_amount,1,mean)
plot(1:t,median_low_mean_amount,type = 'l',lwd=2,ylim = c(min(median_low_mean_amount),max(median_low_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'median_low_test')

median_median_test <- vector("list")
median_median_amount <- matrix(0,t,r)
for(i in 1:r){
  median_median_test[[i]] <- microbe_test(n,t,10,30,1,1,10,1)
  median_median_amount[,i] <- median_median_test[[i]][,1]
}
median_median_mean_amount <- apply(median_median_amount,1,mean)
plot(1:t,median_median_mean_amount,type = 'l',lwd=2,ylim = c((1/2)*min(median_median_mean_amount),max(median_median_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'median_median_test')

median_high_test <- vector("list")
median_high_amount <- matrix(0,t,r)
for(i in 1:r){
  median_high_test[[i]] <- microbe_test(n,t,10,30,1,1,10,2)
  for(j in 1:length(median_high_test[[i]][,1])){
    median_high_amount[j,i] <- median_high_test[[i]][,1][j]
  }
}
median_high_mean_amount <- apply(median_high_amount,1,mean)
plot(1:t,median_high_mean_amount,type = 'l',lwd=2,ylim = c(0,max(median_high_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'median_high_test')

high_low_test <- vector("list")
high_low_amount <- high_low_age <- matrix(0,t,r)
for(i in 1:r){
  high_low_test[[i]] <- microbe_test(n,t,10,30,2,0.5,10,0.5)
  high_low_amount[,i] <- high_low_test[[i]][,1]
}
high_low_mean_amount <- apply(high_low_amount,1,mean)
plot(1:t,high_low_mean_amount,type = 'l',lwd=2,ylim = c(min(high_low_mean_amount),max(high_low_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'high_low_test')

high_median_test <- vector("list")
high_median_amount <- high_median_age <- matrix(0,t,r)
for(i in 1:r){
  high_median_test[[i]] <- microbe_test(n,t,10,30,2,0.5,10,1)
  high_median_amount[,i] <- high_median_test[[i]][,1]
}
high_median_mean_amount <- apply(high_median_amount,1,mean)
plot(1:t,high_median_mean_amount,type = 'l',lwd=2,ylim = c(min(high_median_mean_amount),max(high_median_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'high_median_test')

high_high_test <- vector("list")
high_high_amount <- high_high_age <- matrix(0,t,r)
for(i in 1:r){
  high_high_test[[i]] <- microbe_test(n,t,10,30,2,0.5,10,2)
  high_high_amount[,i] <- high_high_test[[i]][,1]
}
high_high_mean_amount <- apply(high_high_amount,1,mean)
plot(1:t,high_high_mean_amount,type = 'l',lwd=2,ylim = c((1/2)*min(high_high_mean_amount),max(high_high_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'high_high_test')
par(mfrow=c(1,1))
}

