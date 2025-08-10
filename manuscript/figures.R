pal <- RColorBrewer::brewer.pal(8, "Dark2")

## Resource based movement mechanism
sd <- c(0.5, 1, 1.5)
x <- seq(0, 2 * pi, length.out = 365)


# png("Figure2_resource_based_movement.png", width = 2400, height = 1800, res = 300)
# par(bty = "l", mar = c(5, 4, 1, 2))
# plot(NA,
#   type = "n", xlim = c(0, 2 * pi), ylim = c(0, 0.9),
#   xaxt = "n", xlab = expression("Angle"), ylab = "Probability Density"
# )
# axis(1,
#   at = seq(0, 2 * pi, length.out = 9),
#   labels = expression(0, theta / 4, theta / 2, 3 * theta / 4, theta, 5 * theta / 4, 3 * theta / 2, 7 * theta / 4, 2 * theta)
# )
# legend_labels <- sapply(sd, function(s) bquote(sigma == .(s)))
# legend(3 * pi / 2, 0.85, legend = legend_labels, col = pal[seq_along(sd)], lwd = 2, bty = "n", title = "Resource attraction\nfactor")

# for (i in seq_along(sd)) {
#   lines(x, dnorm(x, mean = pi, sd = sd[i]), col = pal[i], lwd = 2)
# }
# dev.off()

## Density based movement mechanism
png("Figure3_density_based_movement.png", width = 2000, height = 2000, res = 300)
par(mar = c(0,0,0,0), lwd = 1.5)
plot(NA, type = "n", xlim = c(-5, 5), ylim = c(-5, 5), axes = FALSE, xlab = "", ylab = "")

theta <- seq(0, 2 * pi, length.out = 365)
radius_outter <- 5
cir_outer_x <- radius_outter * cos(theta)
cir_outer_y <- radius_outter * sin(theta)
polygon(cir_outer_x, cir_outer_y, border = pal[2])

radius_inner <- 3
cir_inner_x <- radius_inner * cos(theta)
cir_inner_y <- radius_inner * sin(theta)
polygon(cir_inner_x, cir_inner_y, border = pal[1])


alpha <- seq(pi / 6, 2 * pi / 6, length.out = 60)
segment_x <- c(0, radius_inner * cos(alpha))
segment_y <- c(0, radius_inner * sin(alpha))
polygon(segment_x, segment_y, col = pal[1], border = NA)

alpha_arc <- seq(0, pi / 4, length.out = 45)
arc_alpha_x <- radius_inner * cos(alpha_arc)
arc_alpha_y <- radius_inner * sin(alpha_arc)
lines(arc_alpha_x, arc_alpha_y, col = pal[4], lty = 1)
text(3.2, 1.3, expression(pi / 4), col = pal[4], cex = 1.5)



points(0, 0, pch = 19, cex = 1.5, col = pal[3])
text(0, 0, "A", col = pal[3], cex = 1.5, pos = 2, offset = 0.75)

points(1.5, 1.5, pch = 19, cex = 1.5, col = pal[4])
segments(0, 0, radius_outter * cos(pi / 4), radius_outter * sin(pi / 4), col = pal[4], lty = 2)
text(1.5, 1.5, "B", col = pal[4], cex = 1.5, pos = 3, offset = 0.75)

points(1.5, 3.5, pch = 19, cex = 1.5, col = pal[5])
text(1.5, 3.5, "C", col = pal[5], cex = 1.5, pos = 3, offset = 0.75)


segments(0, 0, radius_inner, 0, col = 1, lty = 2)
text(radius_inner/2, 0, "radius = 1", col = 1, cex = 1, pos = 1)

radius_arc <- 1
theta_arc <- seq(0, pi / 6, length.out = 30)
arc_x <- radius_arc * cos(theta_arc)
arc_y <- radius_arc * sin(theta_arc)
lines(arc_x, arc_y, col = pal[6], lty = 1)
text(1.3, 0.3, expression(pi / 6), col = pal[6])
dev.off()