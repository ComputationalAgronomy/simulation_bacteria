test_that("if the background microbes move by rules",{
  background_list <- lapply(1:10, function(x){
    background_microbe(speed = runif(1,0,1), location=c(runif(1,0,1),runif(1,0,1)))
  })
  x <- y <- vector("numeric")
  for(i in 1:length(background_list)){
    x[i] <- background_list[[i]]$location[1]
    y[i] <- background_list[[i]]$location[2]
  }
  d <- (x[1]-x[-1])^2+(y[1]-y[-1])^2
  n1 <- sum(x[-1] > x[1] & x[-1] < x[1]+1 & y[-1] > y[1] & y[-1] < y[1]+1 & d<1)
  n2 <- sum(x[-1] > x[1]-1 & x[-1] < x[1] & y[-1] > y[1] & y[-1] < y[1]+1 & d<1 )
  n3 <- sum(x[-1] > x[1]-1 & x[-1] < x[1] & y[-1] > y[1]-1 & y[-1] < y[1] & d<1 )
  n4 <- sum(x[-1] > x[1] & x[-1] < x[1]+1 & y[-1] > y[1]-1 & y[-1] < y[1] & d<1 )
  
  t1 <- t2 <- t3 <- t4 <- t5 <- 0
  iter <- 1
  t <- vector("numeric")
  while(iter <= 20000){
    t <- move.background(background_list[[1]],n1,n2,n3,n4)$location
    tx <- t[1]
    ty <- t[2]
    if(tx > x[1] & tx < x[1]+1 & ty > y[1] & ty < y[1]+1){
      t1 <- t1+1
    }
    if(tx > x[1]-1 & tx < x[1] & ty > y[1] & ty < y[1]+1){
      t2 <- t2+1
    } 
    if(tx > x[1]-1 & tx < x[1] & ty > y[1]-1 & ty < y[1]){
      t3 <- t3+1
    } 
    if(tx > x[1] & tx < x[1]+1 & ty > y[1]-1 & ty < y[1]){
      t4 <- t4+1
    }
    if(tx == x[1] & ty==y[1]){
      t5 <- t5+1
    } 
    iter <- iter+1
  }
  w <- (4/5)/(2/(n1+1)+1/(n2+1)+1/(n3+1)+1/(n4+1))
  p1 <- w*2/(n1+1)
  p2 <- w/(n2+1)
  p3 <- w/(n3+1)
  p4 <- w/(n4+1)
  p5 <- 0.2
  expect_equal(c(t1,t2,t3,t4,t5)/20000,c(p1,p2,p3,p4,0.2),tolerance = 0.05)
})


