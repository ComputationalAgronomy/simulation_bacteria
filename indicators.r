summary.microbe <- function(m){
  output <- sprintf("ID: %d\tAge: %d\tSpeed: %.01f\tLoc: %.04f, %.04f\n", m$id, m$age, m$speed, m$location[1], m$location[2])
  return(cat(output))
}


age_print.microbe <- function(m,n){
  a <- vector("numeric")
  for (i in 1:n){
    a[i] <- m[[i]]$age
  }
  return(hist(a,xlab = "age"))
} 


loc_print.microbe <- function(m,n){
  a <- b <- vector("numeric")
  for(i in 1:n){
    a[i] <- c(m[[i]]$location[1])
    b[i] <- c(m[[i]]$location[2])
    c <- cbind(a,b)
  }
  return(plot(c,main="all_microbe distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} 


loc_time_print.microbe <- function(m,id,t){
  a <- b <- vector("numeric")
  for(i in 1:t){
    a[i] <- c(m[[i]][[id]]$location[1])
    b[i] <- c(m[[i]][[id]]$location[2])
    c <- cbind(a,b)
  } 
  return(plot(c,main="age-loc distribution",xlab="X",ylab="Y",cex=0.5,pch=18))
} 