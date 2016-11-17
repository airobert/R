
library('fields')
library('deSolve')


# params = c(1,2,-2,-1)
# params = c(0.7 , 0.8, 3 , 0)
params = c(1.7 , 0.8, -50 , 2, 0.4)


dvdr = function(v, r, params) { # the vector contains the parameters a, b, c, d
	a = params[1]
	b = params[2]
	c = params[3]
	I = params[5]
	# return a vector contain dv and dr
	d_v = 0.04 * (v^2) + 5 * v + 140 -r + I  
	d_r = a * (b * v - r)
	return (c(d_v, d_r))
}





v_nullcline = function (vv, params){
	a = params[1]
	b = params[2]
	c = params[3]
	I = params[5]
	rr = rep (0, length(vv))
	for (i in 1:length(vv)){
		rr[i] = 0.04 * (v^2) + 5 * v + 14 + I 
	}
	return (rr)
}


r_nullcline = function (vv, params){
	a = params[1]
	b = params[2]
	c = params[3]
	I = params[5]
	rr = rep (0, length(vv))
	for (i in 1:length(vv)){
		rr[i] = b * v
	}
	return (rr)
}







x = seq(0,30, 0.5)
y = rep(0, length(x))
dx = sin(x)

plot (x,y, type='n')
# plot (x,dx, type='n')
arrow.plot(x, 0, dx, 0, arrow.ex = 0.05, length = 0.05, col = 'blue', lwd = 1, true.angle=TRUE)



xmin = -40; xmax = 40; xstep = 5
ymin = -40; ymax = 40; ystep = 5


x = seq(xmin, xmax, xstep)
y = seq(ymin, ymax, ystep)

x_values = rep (x, time = length(y))
y_values = rep (y, each = length(x))



x_dirs = rep (0, length(x_values))
y_dirs = rep (0, length(y_values))



for (i in 1:length(x_values)) {
	dir = dvdr(x_values[i], y_values [i], params)
	x_dirs [i] = dir[1]
	y_dirs [i] = dir[2]
}

plot (x,y, type='n', xlab = 'dx', ylab = 'dy')
arrow.plot(x_values, y_values, x_dirs, y_dirs, arrow.ex = 0.05, length = 0.05, col = 'blue', lwd = 1, true.angle=TRUE)



x = seq(xmin, xmax, xstep/10)
y_x = v_nullcline(x, params)
y_y = r_nullcline(x, params)


lines (x, y_x, col = 'blue',lty=1,lwd=3)
lines (x, y_y, col = 'red',lty=1,lwd=3)




ode_system = function (t, state, params) {
	v = state [1]
	r = state [2]
	return (list(dvdr(v,r,params)))
}


root = function (t, state, params) {
	v = state[1]
	return (v-30)
}

event = function(t, state, params) {
	v = state [1]
	u = state [2]
	c = params [3]
	d = params [4]
	v = c
	u = u+d
	return (c(v,u) )
}




x_iter = seq(xmin, xmax, xstep)
y_iter = seq(ymin, ymax, ystep)

for (x_init in x_iter) {
	for (y_init in y_iter) {

		times = seq(1, 5, 0.05)

		points (x_init, y_init)

		# trajectory <- ode (y = c(y1=x_init, y2=y_init), 
		# 	times = times,
		# 	func = ode_system, 
		# 	parms = params)

		trajectory = ode (y = c(y1= x_init, y2 = y_init),
			func = ode_system,
			times = times,
			parms = params,
			events = list(func = event, root = TRUE),
			rootfun = root)

		lines(trajectory[,2], trajectory[,3], col = 43)
		
	}
}


times = seq(1, 300, 0.01)

points (1.1994, -0.62426)

trajectory = ode (y = c(y1= x_init, y2 = y_init),
			func = ode_system,
			times = times,
			parms = params,
			events = list(func = event, root = TRUE),
			rootfun = root)

lines(trajectory[,1], trajectory[,2], col = 98)


plot.new()
frame()

ts = trajectory[,1]
vs = trajectory[,2]

plot (ts,vs, type='n')
lines(ts, vs, col = 98)


