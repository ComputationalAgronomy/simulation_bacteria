library(ggplot2)
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggtitle
#'@export

## Get the distribution of all the microbes.
age_print.microbe <- function(m){
  a <- vector("numeric")
  for (i in 1:length(m)){
    a[i] <- m[[i]]$age
  }
  b <- data.frame(table(a))
  colnames(b) <- c("age","value")
  p <- ggplot(data = b, aes(x = age, y = value)) +
    geom_bar(stat = "identity",width = 0.05)+
    ggtitle("Age distribution")
  return(p)
} 

## Get the location of all the microbes.
loc_print.microbe <- function(m){
  a <- b <- vector("numeric")
  for(i in 1:length(m)){
    a[i] <- c(m[[i]]$location[1])
    b[i] <- c(m[[i]]$location[2])
    c <- cbind(a,b)
  }
  return(plot(c,main="all_microbe distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} 

## Track the location of an individual. 
loc_time_print.microbe <- function(m,id,time){
  a <- b <- vector("numeric")
  for(i in 1:time){
    a[i] <- c(m[[i]][[id]]$location[1])
    b[i] <- c(m[[i]][[id]]$location[2])
    c <- cbind(a,b)
  } 
  return(plot(c,main="age-loc distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} 

## Generate function : n individuals,t iterations. t:iterations
## p,st_birth,mean,sd: parameters of birth.
## st_death,b: parameters of death.
## ex:Generate and export a table with initial 5 individuals for 69 iterations.
microbe_generate <- function(n,t,p,st_birth,mean,sd,st_death,b){
microbe_list <- lapply(1:n, function(x){
  new_microbe(speed = runif(1, 1,5), location=c(runif(1,0,10),runif(1,0,10)))
})
id <- old <- x <- y <- parent <- amount <- average_age <- vector("numeric")
microbe_table <- vector("list")
iter <- 1
while(iter <= t){
  microbe_list <- lapply(microbe_list,age)
  microbe_list <- lapply(microbe_list, move)
  logic <- lapply(microbe_list,death,st_death,b)
  survive <- which(logic == "True")
  microbe_list <- microbe_list[survive]
  microbe_list <- c(microbe_list,lapply(microbe_list,birth,p,st_birth,mean,sd))
  microbe_list <- microbe_list[!is.na(microbe_list)]
  id <- old <- x <- y <- parent <- amount <- average_age <- vector("numeric")  
  l <- length(microbe_list)
 if(l == 0){
  print(paste("All microbes die at i th iteration where i = ", iter))
  break
}else{
  for(i in 1:l){
    id[i]<- microbe_list[[i]]$id
    old[i] <- microbe_list[[i]]$age
    x[i] <- microbe_list[[i]]$location[1]
    y[i] <- microbe_list[[i]]$location[2]
    parent[i] <- microbe_list[[i]]$parent
    microbe_table[[iter]] <- data.frame(id,old,x,y,parent)
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



##  microbe_generate(100,100,1,10,3,1,30,1)