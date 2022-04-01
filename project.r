rm(list=ls())
m <- matrix(rep(0,121),nrow=11,ncol=11)
m[5,5] <- 1000
iter <- 1
while(iter < 1000){
 for (i in 2:10){
  for (j in 2:10){
    if(m[i,j] > 0){
      m <- move(m,i,j,r,s)
      iter <- iter+1
    }
    }
  }    
}

move <- function(m,i,j,r,s){
  r <- sample(1:m[i,j],1)
  s <- runif(r)
  go_up <- sum(s < 0.125)
  go_down <- sum(s >= 0.125 & s < 0.25)
  go_left <- sum(s >= 0.25 & s < 0.375)
  go_right <- sum(s >= 0.375 & s < 0.5)
  go_ul <- sum(s >= 0.5 & s < 0.625)
  go_ur <- sum(s >= 0.625 & s < 0.75)
  go_dl <- sum(s >= 0.75 & s < 0.875)
  go_dr <- sum(s >= 0.875)
  m[i-1,j] <- m[i-1,j]+go_up
  m[i+1,j] <- m[i+1,j]+go_down
  m[i,j-1] <- m[i,j-1]+go_left
  m[i,j+1] <- m[i,j+1]+go_right
  m[i-1,j-1] <- m[i-1,j-1]+go_ul
  m[i-1,j+1] <- m[i-1,j+1]+go_ur
  m[i+1,j+1] <- m[i+1,j+1]+go_dr
  m[i+1,j-1] <- m[i+1,j-1]+go_dl
  m[i,j] <- m[i,j]-r
  return(m)
}

