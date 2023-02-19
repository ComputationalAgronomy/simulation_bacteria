## No preference involved, strength of density ranked by radius = [0,1,2], 
## of which radius = 0 means no density involved. 
## initial location within square(2,2),speed within (0,1)
## ex. t1 = 10, t2 = 50, t3 = 100
## ex.move_test_density(200,20,0,1,2,10,50,100)
move_test_general <- function(n,boundary,radius1,radius2,radius3,t1,t2,t3){
  par(mfrow=c(3,3))
  general_movement(n,t1,boundary,radius1)
  general_movement(n,t2,boundary,radius1)
  general_movement(n,t3,boundary,radius1)
  general_movement(n,t1,boundary,radius2)
  general_movement(n,t2,boundary,radius2)
  general_movement(n,t3,boundary,radius2)
  general_movement(n,t1,boundary,radius3)
  general_movement(n,t2,boundary,radius3)
  general_movement(n,t3,boundary,radius3)
  par(mfrow=c(1,1))
}

## With preference & density involved
move_test_background <- function(n,boundary=10,radius1,radius2,radius3,t1,t2,t3,resource=c(5,5)){
  par(mfrow=c(3,3))
  background_movement(n,t1,boundary,radius1,resource)
  background_movement(n,t1,boundary,radius2,resource)
  background_movement(n,t1,boundary,radius3,resource)
  background_movement(n,t2,boundary,radius1,resource)
  background_movement(n,t2,boundary,radius2,resource)
  background_movement(n,t2,boundary,radius3,resource)
  background_movement(n,t3,boundary,radius1,resource)
  background_movement(n,t3,boundary,radius2,resource)
  background_movement(n,t3,boundary,radius3,resource)
  par(mfrow=c(1,1))
}


