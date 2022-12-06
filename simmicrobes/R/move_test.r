move_test_background <- function(n,boundary = 100,t1,t2,t3){
  par(mfrow=c(1,3))
  background_movement(n,boundary,t1)
  background_movement(n,boundary,t2)
  background_movement(n,boundary,t3)
  par(mfrow=c(1,1))
}


