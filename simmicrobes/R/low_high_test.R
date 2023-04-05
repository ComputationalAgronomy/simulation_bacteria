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
  logic <- lapply(microbe_list,death,st_death,death_rate,length(microbe_list),capacity)
  survive <- which(logic == "True")
  microbe_list <- microbe_list[survive]
  microbe_list <- c(microbe_list,lapply(microbe_list,birth,st_birth,birth_rate,length(microbe_list),capacity))
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

logistic_curve <- function(x, r, K, N0) {
  Nt <- K / (1 + ((K - N0) / N0) * exp(-r * x))
  plot(x, Nt, type = 'l', xlab = "Time", ylab = "Population Size",lty = 1,lwd = 5)
  iter <- 1
  while(iter < 10){
  mt <- microbe_test(100,30,1,0.5,1,0,1000)
  lines(1:30,mt[,1],type = 'l', col = 'lightblue',lty = 1,lwd = 2)
  iter <- iter+1
  }
  lines(x, Nt, type = 'l',lty = 1,lwd = 5)
  legend("topleft",legend = c("Theoretical","Simulated"),col=c("black","lightblue"),lty=c(1,1))
}

## logistic_curve(1:30,0.5,1000,100)



## n = initial population 
## t = iteration(age)
## r = replication

amount_test <- function(n,t){
 par(mfrow=c(3,3))
t11 <- microbe_test(n,t,1,0.1,1,0.1,1000)
plot(1:t,t11[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.1,d = 0.1")
t12 <- microbe_test(n,t,1,0.5,1,0.1,1000)
plot(1:t,t12[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.5,d = 0.1")
t13 <- microbe_test(n,t,1,0.9,1,0.1,1000)
plot(1:t,t13[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.9,d = 0.1")
t21 <- microbe_test(n,t,1,0.1,1,0.5,1000)
plot(1:t,t21[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.1,d = 0.5")
t22 <- microbe_test(n,t,1,0.5,1,0.5,1000)
plot(1:t,t22[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.5,d = 0.5")
t23 <- microbe_test(n,t,1,0.9,1,0.5,1000)
plot(1:t,t23[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.9,d = 0.5")
t31 <- microbe_test(n,t,1,0.1,1,0.9,1000)
plot(1:t,t31[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.1,d = 0.9")
t32 <- microbe_test(n,t,1,0.5,1,0.9,1000)
plot(1:t,t32[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.5,d = 0.9")
t33 <- microbe_test(n,t,1,0.9,1,0.9,1000)
plot(1:t,t33[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000),
     main="b = 0.9,d = 0.9")
par(mfrow=c(1,1))
}

Lag_test <- function(n,t = 30){
  m1 <- microbe_test(n,t,1,0.5,1,0.5,1000)
  plot(1:t,m1[,1],type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',ylim=c(100,1000))
  m2 <- microbe_test(n,t,2,0.5,1,0.5,1000)
  lines(1:30,m2[,1],lwd=2,col = 'blue')
  m3 <- microbe_test(n,t,1,0.5,2,0.5,1000)
  lines(1:30,m3[,1],lwd=2,col = 'red')
  legend("topleft",legend = c("Normal","Lag in birth","Lag in death"),lty=c(1,1,1),col=c("black","blue","red"))
}

Real_curve <- function(n,t1=40,t2=60,capacity=1000){
  microbe_list <- lapply(1:n, function(x){
    new_microbe(speed = runif(1,1,5), location=c(0,0))
  })
  l <- c()
  iter <- 1
  while(iter <= t1){
    microbe_list <- lapply(microbe_list,age)
    logic <- lapply(microbe_list,death,1,0,length(microbe_list),1000)
    survive <- which(logic == "True")
    microbe_list <- microbe_list[survive]
    microbe_list <- c(microbe_list,lapply(microbe_list,birth,1,0.5,length(microbe_list),1000))
    microbe_list <- microbe_list[!is.na(microbe_list)]
    l[iter] <- length(microbe_list)
    iter <- iter+1
  }
  while(iter <= t2){
    microbe_list <- lapply(microbe_list,age)
    logic <- lapply(microbe_list,death,1,0.5,length(microbe_list),1000)
    survive <- which(logic == "True")
    microbe_list <- microbe_list[survive]
    if(length(microbe_list) == 0){
      print(paste("All microbes die at i th iteration where i = ", iter))
      break
    }
    l[iter] <- length(microbe_list)
    iter <- iter+1
  }
  return(plot(1:t2,l,type = 'l',lwd=2,xlab = 'Time',ylab = 'Population size',
              ylim=c(100,1000), main = "Simulated curve"))
  }





