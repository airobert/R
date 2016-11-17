
library('fields')
library('deSolve')

# x = seq(0,30, 0.5)
# y = rep(0, length(x))
# dx = 2 - 0.1 *x

# plot (x,y, type='n')
# arrow.plot(x, 0, dx, 0, arrow.ex = 0.05, length = 0.05, col = 'blue', lwd = 1, true.angle=TRUE)







x = seq(0,30, 0.5)
y = rep(0, length(x))
dx = sin(x)

plot (x,y, type='n')
# plot (x,dx, type='n')
arrow.plot(x, 0, dx, 0, arrow.ex = 0.05, length = 0.05, col = 'blue', lwd = 1, true.angle=TRUE)



xmin = -2; xmax = 2; xstep = 0.2
ymin = -2; ymax = 2; ystep = 0.2


x = seq(xmin, xmax, xstep)
y = seq(ymin, ymax, ystep)

x_values = rep (x, time = length(y))
y_values = rep (y, each = length(x))

# > l = seq(0,1,0.1)
# > l
#  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
# > rep(l, time = 2)
#  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7
# [20] 0.8 0.9 1.0
# > rep(l, each = 2)
#  [1] 0.0 0.0 0.1 0.1 0.2 0.2 0.3 0.3 0.4 0.4 0.5 0.5 0.6 0.6 0.7 0.7 0.8 0.8 0.9
# [20] 0.9 1.0 1.0

dxdy = function(x, y, params) { # the vector contains the parameters a, b, c, d
	a = params[1]
	b = params[2]
	c = params[3]
	d = params[4]
	# return a vector contain dx and dy
	d_x = a * x + b * y
	d_y = c * x + d * y
	return (c(d_x, d_y))
}

x_dirs = rep (0, length(x_values))
y_dirs = rep (0, length(y_values))

params = c(1,2,-2,-1)

for (i in 1:length(x_values)) {
	dir = dxdy(x_values[i], y_values [i], params)
	x_dirs [i] = dir[1]
	y_dirs [i] = dir[2]
}

plot (x,y, type='n', xlab = 'dx', ylab = 'dy')
arrow.plot(x_values, y_values, x_dirs, y_dirs, arrow.ex = 0.05, length = 0.05, col = 'blue', lwd = 1, true.angle=TRUE)


x_nullcline = function (xx, params){
	a = params[1]
	b = params[2]
	c = params[3]
	d = params[4]
	yy = rep (0, length(xx))
	for (i in 1:length(xx)){
		yy[i] = -1* a / b * xx[i]
	}
	return (yy)
}


y_nullcline = function (xx, params){
	a = params[1]
	b = params[2]
	c = params[3]
	d = params[4]
	yy = rep (0, length(xx))
	for (i in 1:length(xx)){
		yy[i] = -1* c / d * xx[i]
	}
	return (yy)
}

x = seq(xmin, xmax, xstep/10)
y_x = x_nullcline(x, params)
y_y = y_nullcline(x, params)


lines (x, y_x, col = 'blue',lty=1,lwd=3)
lines (x, y_y, col = 'red',lty=1,lwd=3)

ode_system = function (t, state, params) {
	x = state [1]
	y = state [2]
	a = params[1]
	b = params[2]
	c = params[3]
	d = params[4]
	return (list(c(a*x + b*y, c*x + d*y)))
}


x_iter = seq(xmin, xmax, xstep* 2)
y_iter = seq(ymin, ymax, ystep* 2)

for (x_init in x_iter) {
	for (y_init in y_iter) {

		times = seq(1, 5, 0.01)

		points (x_init, y_init)

		trajectory <- ode (y = c(y1=x_init, y2=y_init), 
			times = times,
			func = ode_system, 
			parms = params)

		lines(trajectory[,2], trajectory[,3], col = 43)
		
	}
}



