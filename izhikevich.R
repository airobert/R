##############################################
# Use this part of the file to set the parameters/initialise values

library('fields')
library('deSolve')

# set values for parameters (a, b and c and d or I)
# a <- 0.02; b <- 0.2; c <- -65; d <- 8; I=10; # regular
a <- 0.1; b <- 0.2; c <- -55; d <- 4; I=10; # fast
# a <- 0.02; b <- 0.2; c <- -50; d <- 4; I=10; # Intrinsically bursting
params <- c(a, b, c, d, I)                  
v_init <- -65; u_init <- -18                # initial values of v and u
vmin <- -80; vmax <- 40; vstep <- 5         # set min, max and stepsize for v
umin <- -30; umax <- 30; ustep <- 2.5       # set min, max and stepsize for u

times <- seq(1, 200, 0.1)                   # test times

# create v and r values
v <- seq(vmin, vmax, vstep)                 # create v values
u <- seq(umin, umax, ustep)                 # create u values
v_values <- rep(v, time = length(u))        # create vector with duplicated v values
u_values <- rep(u, each = length(v))        # create vector with duplicated u values

dvdu <- function(v, u, params) {
   a <- params[1]; b <- params[2]; c <- params[3]; d <- params[4]; I <- params[5]

    # reset condition
    if (v >= 30) {
        v <- c
        u <- u + d
    }

	return(c(0.04 * v^2 + 5*v + 140 - u + I, a * (b*v - u)))
}

v_dirs = rep(0, length(v_values))            # initialise v_dir
u_dirs = rep(0, length(u_values))            # initialise u_dir

# Loop through (v, u) points and fill v_dir and u_dir
for (i in 1:length(v_values)) {
    dir <- dvdu(v_values[i], u_values[i], params)
    v_dirs[i] = dir[1]
    u_dirs[i] = dir[2]
}

# plot phase portrait
plot(v_values, u_values, type='n', xlab="v", ylab="u")
arrow.plot(v_values, u_values, v_dirs, u_dirs,
     arrow.ex=0.05, length=.05, col='blue', lwd=1,
     true.angle = TRUE)

# nullcline functions
v_nullcline = function(v, p) {
    a < p[1]; I <- p[5]
    return(c(0.04*v^2 + 5*v + 140 + I))
}

u_nullcline = function(v, p) {
    b <- p[2]; c <- p[3]
    return(c(b*v))
}

# generate data to plot nullclines
v <- seq(vmin, vmax, vstep/10)          # create v-values, use vmin and vmax from before
u_v = v_nullcline(v, params)            # generate y values for v-nullcline
u_u = u_nullcline(v, params)            # generate r values for r-nullcline
 
# plot nullclines
lines(v, u_v, col='red')
lines(v, u_u, col='red')

# plot reset condition
lines(c(30, 30), c(umin-2, umax+2), col='blue', lwd=7)

# plot trajectories

# redefine system for ode
ode_system <- function(t, state, params) {
    v <- state[1]; u <- state[2]
    return(list(dvdu(v, u, params)))
}
# 
# root function
root <- function(t, state, params) {
    v <- state[1]
    return(v - 30)
}

# event function
event <- function(t, state, params) {
	v <- state[1]; u <- state[2]
	c <- params[3]; d <- params[4]
	v <- c
	u <- u + d
	return(c(v,u))
}


# compute trajectory
points(v_init, u_init)                  # add starting point to plot
trajectory <- ode(y = c(y1=v_init, y2=u_init), 

                  func=ode_system,
                  times = times,
                  parms = params,
                  events = list(func=event, root=TRUE), rootfun=root)

lines(trajectory[,2], trajectory[,3], col='black')

# dev.copy(png, filename="~/Dropbox/fncm15/computer_labs/week2/RS_vt.png")
# dev.off()

plot(trajectory[,1], trajectory[,2], xlab="t", ylab="v", col='black', type='l')

# dev.copy(png, filename="~/Dropbox/fncm15/computer_labs/week2/RS_pp.png")
# dev.off()



# x_iter = seq(xmin, xmax, xstep)
# y_iter = seq(ymin, ymax, ystep)

# for (x_init in x_iter) {
#   for (y_init in y_iter) {

#     times = seq(1, 5, 0.05)

#     points (x_init, y_init)

#     # trajectory <- ode (y = c(y1=x_init, y2=y_init), 
#     #   times = times,
#     #   func = ode_system, 
#     #   parms = params)

#     trajectory = ode (y = c(y1= x_init, y2 = y_init),
#       func = ode_system,
#       times = times,
#       parms = params,
#       events = list(func = event, root = TRUE),
#       rootfun = root)

#     lines(trajectory[,2], trajectory[,3], col = 43)
    
#   }
# }

