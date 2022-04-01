m <- matrix(c(1,rep(0,35)),nrow=6,ncol=6)
  for( i in 1:nrow(m)-1){
    for( j in 1:ncol(m)-1){
      if (m[i,j] == 1 && i >= 1 && i < 6 && j >=1 && j < 6){
        m[i+1,j+1] <- m[i+1,j+1]+1
        m[i,j] <- m[i,j]-1
      }
      }
    }


rm(list=ls())
m <- matrix(c(1,rep(0,35)),nrow=6,ncol=6)
iter <- 1
r <- runif(1)
while(iter < 100){
  for (i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      
      if(m[i,j] > 0 && i > 1 && i < 6 && j > 1 && j < 6){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r*0.25
        m[i-1,j] <- m[i-1,j] + m[i,j]*r*0.25
        m[i,j+1] <- m[i,j+1] + m[i,j]*r*0.25
        m[i,j-1] <- m[i,j-1] + m[i,j]*r*0.25
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i == 1 && j > 1 && j < 6){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r*(1/3)
        m[i,j+1] <- m[i,j+1] + m[i,j]*r*(1/3)
        m[i,j-1] <- m[i,j-1] + m[i,j]*r*(1/3)
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i == 6 && j > 1 && j < 6){
        m[i-1,j] <- m[i-1,j] + m[i,j]*r*(1/3)
        m[i,j+1] <- m[i,j+1] + m[i,j]*r*(1/3)
        m[i,j-1] <- m[i,j-1] + m[i,j]*r*(1/3)
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i > 1 && i < 6 && j == 1){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r*(1/3)
        m[i-1,j] <- m[i-1,j] + m[i,j]*r*(1/3)
        m[i,j+1] <- m[i,j+1] + m[i,j]*r*(1/3)
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i > 1 && i < 6 && j==6){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r*(1/3)
        m[i-1,j] <- m[i-1,j] + m[i,j]*r*(1/3)
        m[i,j-1] <- m[i,j-1] + m[i,j]*r*(1/3)
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i == 1 && j == 1){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r*0.5
        m[i,j+1] <- m[i,j+1] + m[i,j]*r*0.5
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i == 1 && j == 6){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r*0.5
        m[i,j-1] <- m[i,j-1] + m[i,j]*r*0.5
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i == 6 && j == 1){
        m[i-1,j] <- m[i-1,j] + m[i,j]*r*0.5
        m[i,j+1] <- m[i,j+1] + m[i,j]*r*0.5
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
      
      if(m[i,j] > 0 && i == 6 && j == 6){
        m[i-1,j] <- m[i-1,j] + m[i,j]*r*0.5
        m[i,j-1] <- m[i,j-1] + m[i,j]*r*5
        m[i,j] <- m[i,j]-m[i,j]*r
        iter <- iter+1
      }
    }
  }
}

sum(m)






# why missing value generated? 
rm(list=ls())
  m <- matrix(c(1,rep(0,35)),nrow=6,ncol=6)
  iter <- 1
  r <- runif(100)
  while(iter < 100){
    for (i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      
    if(m[i,j] > 0 && i > 1 && i < 6 && j > 1 && j < 6){
      m[i+1,j] <- m[i+1,j] + m[i,j]*r[iter]*0.25
      m[i-1,j] <- m[i-1,j] + m[i,j]*r[iter]*0.25
      m[i,j+1] <- m[i,j+1] + m[i,j]*r[iter]*0.25
      m[i,j-1] <- m[i,j-1] + m[i,j]*r[iter]*0.25
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
    if(m[i,j] > 0 && i == 1 && j > 1 && j < 6){
      m[i+1,j] <- m[i+1,j] + m[i,j]*r[iter]*(1/3)
      m[i,j+1] <- m[i,j+1] + m[i,j]*r[iter]*(1/3)
      m[i,j-1] <- m[i,j-1] + m[i,j]*r[iter]*(1/3)
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
    if(m[i,j] > 0 && i == 6 && j > 1 && j < 6){
      m[i-1,j] <- m[i-1,j] + m[i,j]*r[iter]*(1/3)
      m[i,j+1] <- m[i,j+1] + m[i,j]*r[iter]*(1/3)
      m[i,j-1] <- m[i,j-1] + m[i,j]*r[iter]*(1/3)
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
    if(m[i,j] > 0 && i > 1 && i < 6 && j == 1){
      m[i+1,j] <- m[i+1,j] + m[i,j]*r[iter]*(1/3)
      m[i-1,j] <- m[i-1,j] + m[i,j]*r[iter]*(1/3)
      m[i,j+1] <- m[i,j+1] + m[i,j]*r[iter]*(1/3)
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
      if(m[i,j] > 0 && i > 1 && i < 6 && j==6){
        m[i+1,j] <- m[i+1,j] + m[i,j]*r[iter]*(1/3)
        m[i-1,j] <- m[i-1,j] + m[i,j]*r[iter]*(1/3)
        m[i,j-1] <- m[i,j-1] + m[i,j]*r[iter]*(1/3)
        m[i,j] <- m[i,j]-m[i,j]*r[iter]
        iter <- iter+1
      }
      
    if(m[i,j] > 0 && i == 1 && j == 1){
      m[i+1,j] <- m[i+1,j] + m[i,j]*r[iter]*0.5
      m[i,j+1] <- m[i,j+1] + m[i,j]*r[iter]*0.5
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
    if(m[i,j] > 0 && i == 1 && j == 6){
      m[i+1,j] <- m[i+1,j] + m[i,j]*r[iter]*0.5
      m[i,j-1] <- m[i,j-1] + m[i,j]*r[iter]*0.5
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
    if(m[i,j] > 0 && i == 6 && j == 1){
      m[i-1,j] <- m[i-1,j] + m[i,j]*r[iter]*0.5
      m[i,j+1] <- m[i,j+1] + m[i,j]*r[iter]*0.5
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
      
    if(m[i,j] > 0 && i == 6 && j == 6){
      m[i-1,j] <- m[i-1,j] + m[i,j]*r[iter]*0.5
      m[i,j-1] <- m[i,j-1] + m[i,j]*r[iter]*0.5
      m[i,j] <- m[i,j]-m[i,j]*r[iter]
      iter <- iter+1
    }
  }
}
    }


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

