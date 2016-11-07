x <- seq(0, 30, 0.5)
y <- rep(0, length(x))
dx <- 2 - 0.1 * x

plot (x, y, type = 'n')

arrow.plot (x, 0, 
	dx, 0, 
	arrow.ex = 0.05, length = 0.05, col='blue', lwd = 1, true.angle = TRUE)

xmin = -2; xmx = -2 ; xstep = -0.2
ymin = -2; ymax = -2; ystep = -0.2

x = seq(xmin, xmax, xstep)
y = seq(ymin, ymax, ystep)

x_values = rep(x, time = length(y))
y_values = rep(y, each = length(y))

