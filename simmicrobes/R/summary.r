library(ggplot2)
#' @importFrom ggplot2
#' @export

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