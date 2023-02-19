test_that("if the background microbes move by rules",{
  back <- background_microbe(speed = runif(1,0,1), location=c(0,0))
  iter <- 1
  angle <- vector("numeric")
  while(iter <= 10000){
    loc <-  move.background(back,c(0,0,0,0),c(2,2))$location
    angle[iter] <- angle.cal(c(2,2),loc)
    iter <- iter+1
  }
  expect_equal(mean(angle),pi/4,tolerance = 0.05)
})

test_that("if the general microbes move by rules",{
  microbe_list <- lapply(1:10, function(x){
    new_microbe(speed = runif(1,0,1), location=c(runif(1,0,1),runif(1,0,1)))
  })
  x <- y <- vector("numeric")
  for(i in 1:length(microbe_list)){
    x[i] <- microbe_list[[i]]$location[1]
    y[i] <- microbe_list[[i]]$location[2]
  }
  den <- density(microbe_list,x,y,1)[1,]
  iter <- 1
  angle <- vector("numeric")
  while(iter <= 10000){
    loc <-  move.general(microbe_list[[1]],den)$location
    angle[iter] <- angle.cal(c(2,2),loc)
    iter <- iter+1
  }
  o1 <- sum(angle > 0 & angle < pi/2)
  o2 <- sum(angle > pi/2 & angle < pi)
  o3 <- sum(angle > pi & angle < 3*pi/2)
  o4 <- sum(angle > 3*pi/2 & angle < 2*pi)
  o5 <- sum(angle = NA)
  expect_equal(c(o1,o2,o3,o4,o5),c(den*9000,1000),tolerance = 0.05)
})

