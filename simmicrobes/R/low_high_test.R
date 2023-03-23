## This file is for test.
## Fix the parameters of birth and death rate function, basically, nine sets are used: low, medium, high birth rate
## and low, medium,high death rate.
## Initial population and iterations are fixed. 
## Ignore movement here.

microbe_test <-  function(n,t,st_birth,birth_rate,st_death,death_rate,capacity){
  microbe_list <- lapply(1:n, function(x){
    new_microbe(speed = runif(1,1,5), location=c(0,0))
  })
  id <- old <- amount <- average_age <- vector("numeric")
  microbe_table <- vector("list")
  iter <- 1
  while(iter <= t){
  microbe_list <- lapply(microbe_list,age)
  microbe_list <- c(microbe_list,lapply(microbe_list,birth,st_birth,birth_rate,length(microbe_list),capacity))
  microbe_list <- microbe_list[!is.na(microbe_list)]
  logic <- lapply(microbe_list,death,st_death,death_rate,length(microbe_list),capacity)
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

logistic_curve_1 <- function(x, r, K, N0) {
  Nt <- K / (1 + ((K - N0) / N0) * exp(-r * x))
  plot(x, Nt, type = "l", xlab = "Time", ylab = "Population Size")
  mt <- microbe_test(100,20,1,0.5,1,0.1,500)
  lines(1:20,mt[,1],type = 'l', col = 'blue')
  legend("topleft",legend = c("empirical","simulated"),col=c("black","blue"),lty=c(1,1))
}
## logistic_curve_1(1:20,0.4,500,100)

## n = initial population 
## t = iteration(age)
## r = replication
amount_test <- function(n,t,r){
par(mfrow=c(1,3))
low_low_test <- vector("list")
low_low_amount <- low_low_age <- matrix(0,t,r)
for(i in 1:r){
  low_low_test[[i]] <- microbe_test(n,t,1,0.2,1,1000)
  low_low_amount[,i] <- low_low_test[[i]][,1]
}
low_low_mean_amount <- apply(low_low_amount,1,mean)
plot(1:t,low_low_mean_amount,type = 'l',lwd=2,ylim = c((1/2)*(min(low_low_mean_amount)),max(low_low_mean_amount)),
     xlab = 'age',ylab = 'population',main = 'low birth rate')

low_median_test <- vector("list")
low_median_amount <- matrix(0,t,r)
for(i in 1:r){
  low_median_test[[i]] <- microbe_test(n,t,n,t,1,0.5,1,1000)
  for(j in 1:length(low_median_test[[i]][,1])){
    low_median_amount[j,i] <- low_median_test[[i]][,1][j]
  }
}
low_median_mean_amount <- apply(low_median_amount,1,mean)
plot(1:t,low_median_mean_amount,type = 'l',lwd=2,ylim = c(0,max(low_median_mean_amount)),
     xlab = 'age',ylab = 'population',main = 'median birth rate')

low_high_test <- vector("list")
low_high_amount <- matrix(0,t,r)
for(i in 1:r){
  low_high_test[[i]] <- microbe_test(n,t,1,0.8,1,1000)
  for(j in 1:length(low_high_test[[i]][,1])){
    low_high_amount[j,i] <- low_high_test[[i]][,1][j]
  }
}
low_high_mean_amount <- apply(low_high_amount,1,mean)
plot(1:t,low_high_mean_amount,type = 'l',lwd=2,ylim = c(0,max(low_high_mean_amount)),
     xlab = 'age',ylab = 'amount',main = 'high birth test')
par(mfrow=c(1,1))
}

