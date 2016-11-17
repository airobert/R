# load required packages
library(fields)
library(deSolve)

# set values for parameters (a, b and c and d or I)
a   <- 0.5;         b  <- 0.8;      c   <-    3;   I   <- -0.2;
params  =    c(a, b, c, I)                  

# set starting points for plotting trajectories
v_init  <-  c(1.1994)
r_init <- c(-0.62426)

# set min, max and stepsize for plotting
vmin    <-  -2;     vmax <- 2;      vstep <-  0.2          
rmin    <-  -2;     rmax <- 2;      rstep <-  0.2              

# set times for trajectory plotting
times <- seq(1, 35, 0.01)                # times to plot


# create v and r values
v <- seq(vmin, vmax, vstep)                 # create v values
r <- seq(rmin, rmax, rstep)                 # create r values
v_values <- rep(v, time = length(r))        # create vector with duplicated v values
r_values <- rep(r, each = length(v))        # create vector with duplicated r values

dvdr <- function(v, r, params) {
    # Write here your function returning a vector with
    # dv/dt and dr/dt
}

v_dirs = rep(0, length(v_values))            # initialise v_dir
r_dirs = rep(0, length(r_values))            # initialise r_dir

# Loop through (v, r) points and fill v_dir and r_dir
# dirs = dvdr(v_values, r_values, params)
for (i in 1:length(v_values)) {
    dir <- dvdr(v_values[i], r_values[i], params)
    v_dirs[i] = dir[1]
    r_dirs[i] = dir[2]
}

# v_dirs = dirs[1:length(v_values)/2]
# r_dirs = dirs[-(1:length(v_values)/2)]

# plot phase portrait
plot(v_values, r_values, type='n', xlab="v", ylab="r")
arrow.plot(v_values, r_values, v_dirs, r_dirs,
     arrow.ex=0.05, length=.05, col='blue', lwd=1,
     true.angle = TRUE)

# nullcline functions
v_nullcline = function(v, p) {
    # Write here your function for the v nullcline
}

r_nullcline = function(v, p) {
    # Write here your function for the r nullcline
}


# generate data to plot nullclines
v <- seq(vmin, vmax, vstep/10)          # create v-values, use vmin and vmax from before
r_v = v_nullcline(v, params)            # generate y values for v-nullcline
r_r = r_nullcline(v, params)            # generate r values for r-nullcline

# plot nullclines
lines(v, r_v, col='red')
lines(v, r_r, col='red')

# plot trajectories


#redefine system for ode
ode_system <- function(t, state, params) {
    v <- state[1]; r <- state[2]
    return(list(dvdr(v, r, params)))
}


# compute trajectories
for (i in 1:length(v_init)) {
    v = v_init[i]; r = r_init[i]
    points(v, r)                          # add starting point to plot
    trajectory <- ode(y = c(y1=v, y2=r), 
                  func=ode_system,
                  times = times,
                  parms = params)

    lines(trajectory[,2], trajectory[,3], col='black')
}
