## Grid research for movement 
resource_test <- function(n,t,boundary,radius,arc,ec,resource){
  par(mfrow=c(2,3))
  g1 <- general_movement(n,t,boundary,sd = NA,radius,arc,ec)
  loc_print.microbe(g1, "No resource")
  g2 <- background_movement(n,t,boundary,sd = 2,radius,arc,ec,c(5,5))
  loc_print.microbe(g2, "sd = 2")
  g3 <- background_movement(n,t,boundary,sd = 1.5,radius,arc,ec,c(5,5))
  loc_print.microbe(g3, "sd = 1.5")
  g4 <- background_movement(n,t,boundary,sd = 1,radius,arc,ec,c(5,5))
  loc_print.microbe(g4, "sd = 1")
  g5 <- background_movement(n,t,boundary,sd = 0.5,radius,arc,ec,c(5,5))
  loc_print.microbe(g5, "sd = 0.5")
  g6 <- background_movement(n,t,boundary,sd = 0.1,radius,arc,ec,c(5,5))
  loc_print.microbe(g6, "sd = 0.1")
  par(mfrow=c(1,1))
}