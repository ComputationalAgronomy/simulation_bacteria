library(devtools)
load_all("simmicrobes")

## set speed to 2
ncpu <- floor(detectCores() * 2 / 3)

density_test_take2 <- function(sd = NA, radius, arc, ec) {

  start <- 0.3
  g1 <- lapply(1:50, function(x) {
    new_microbe(speed = 0.5, location = c(runif(1, 2.5, 7.5), start))
  })
  g2 <- lapply(1:200, function(x) {
    new_microbe(speed = 0, location = c(runif(1, 1, 10), runif(1, -0.2, 0.2)))
  })
  g3 <- lapply(1:50, function(x) {
    new_microbe(speed = 0.5, location = c(runif(1, 2.5, 7.5), -start))
  })

  m_movable <- c(g1, g3)
  m_stationary <- g2

  par(mfrow = c(3, 3))
  # cl <- makeCluster(getOption("cl.cores", 2))



  plot_loc_two_microbes(m_movable, m_stationary, "Age = 0")
  # iter <- 1
  iter_end <- 10
  for(iter in seq_len(iter_end)){
    m_all <- c(m_movable, m_stationary)
    xy_cord <- sapply(m_all, "[[", "location")

    # If all movable: angle_tab <- cal_angle_table(xy_cord, index = seq_len(NCOL(xy_cord)), ncpu = ncpu)
    angle_tab <- calculate_angle_table(xy_cord[1, ], xy_cord[2,], ncpu = ncpu)
    distance_tab <- calculate_dist_table(xy_cord[1, ], xy_cord[2,])

    # m_movable <- parLapply(seq_along(m_movable), mc.cores = ncpu,  function(k) {
    m_movable <- mclapply(seq_along(m_movable), mc.cores = ncpu,  function(k) {
      move(m_movable[[k]], distance_tab[k,], angle_tab[k, ],
        sd, radius, arc, ec)
    })

    if (iter %in% seq(0, iter_end, by=2)){
      plot_loc_two_microbes(m_movable, m_stationary, sprintf("Age = %s", iter))
      abline(h=c(-start, start))
    }
  }

}


sd <- 0
radius <- 1
arc <- pi / 4

# density_test_take2(sd, radius, arc, ec = 1.5)
#          density_test_take2(sd, radius, arc, ec = 0.01)


# density_test_take2(sd, radius, arc, ec = 3)
# density_test_take2(sd, radius, arc, ec = 0.5)
