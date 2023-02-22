test_that("if the background microbes move by rules",{
  back <- background_microbe(speed = runif(1,0,1), location=c(0,0))
  iter <- 1
  angle <- vector("numeric")
  while(iter <= 10000){
    loc <-  move.background(back,c(0),c(0),-1,c(0,1))$location
    angle[iter] <- angle.cal(loc,c(0,0))
    iter <- iter+1
  }
  expect_equal(mean(angle,na.rm = T),pi/2,tolerance = 0.05)
})

test_that("if the density factor makes sense",{
  microbe_list <- lapply(1:3, function(x){
    new_microbe(speed = runif(1,0,1), location=c(1,1))
  })
  microbe_list[[1]]$location <- c(0,0)
  microbe_list[[3]]$location <- c(0,1)
  x <- y <- vector("numeric")
  for(i in 1:length(microbe_list)){
    x[i] <- microbe_list[[i]]$location[1]
    y[i] <- microbe_list[[i]]$location[2]
  }
  distance_table <- distance.table(x,y)
  angle_table <- angle.table(x,y)
  angle <- vector("numeric")
  iter <- 1
  while(iter <= 10000){
    loc <-  move.general(microbe_list[[1]],distance_table[1,],angle_table[1,],2)$location
    angle[iter] <- angle.cal(loc,c(0,0))
    iter <- iter+1
  }
  expect_equal(c(o1,o2,o3,o4,o5),c(den*9000,1000),tolerance = 0.05)
})


sum(angle <= pi/3 & angle >= pi/6, na.rm = T)
sum(angle <= 7*pi/12 & angle >= 5*pi/12, na.rm = T)
sum(angle == 10*pi/12, na.rm = T)
sum(angle <= pi/180 & angle >= 2*pi/180, na.rm = T)


