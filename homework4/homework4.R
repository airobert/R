source("/home/airobert/Project/Rhomework/hopfield.R")

# weights = matrix (c(0,1,-2,1,0,1,-2,1,0),3,3)

# hopnet = list (weights = weights)

# init.y = c(1,-1, 1)
# run.hopfield(hopnet, init.y, stepbystep = T, topo=c(3,1), maxit = 10)

weights = matrix (c(0, -2, 1, 1, 0, -2, -2, 1, 0),3,3)

hopnet = list (weights = weights)

init.y = c(1,1, 1)
run.hopfield(hopnet, init.y, stepbystep = T, topo=c(3,1), maxit = 30)



# 2.2

weights1 = matrix (c(0, -1, 1, 0),2, 2)

hopnet1 = list (weights = weights1)

init1.y = c(1,-1)
run.hopfield(hopnet1, init1.y, stepbystep = T, topo=c(2,1), maxit = 10)



weights2 = matrix (c(0, 1, 1, 0),2, 2)

hopnet2 = list (weights = weights2)

init1.y = c(1,-1)
run.hopfield(hopnet2, init1.y, stepbystep = T, topo=c(2,1), maxit = 10)

pattern1 = rep(1, 10*10)
pattern2 = rep(-1, 10*10)

weights = matrix (rep(0,100*100), 100, 100)
weights = weights + pattern1 %o% pattern1 
		+ pattern2 %o% pattern2
		- 2*diag(10*10);



# interate to see if this network really store the two images

run.hopfield (list(weights=weights), pattern1,
	stepbystep = T, topo=c(10,10), replace = F)

run.hopfield (list(weights=weights), pattern2,
	stepbystep = T, topo=c(10,10), replace = F)


# homework 3

pattern3 = matrix (pattern1, 10,10)

for (i in 1:3)
	pattern3[i,] = -1
dim(pattern3) = NULL

pattern4 = matrix (pattern1, 10,10)

for (i in 1:5)
	pattern4[i,] = -1
dim(pattern4) = NULL

# pattern5 = matrix (pattern1, 10,10)

# for (i in 1:4)
# 	pattern5[i,] = -1
# dim(pattern5) = NULL

# pattern6 = matrix (pattern1, 10,10)

# for (i in 1:6)
# 	pattern6[i,] = -1
# dim(pattern6) = NULL



weights2 = matrix (rep(0,100*100), 100, 100)
weights2 = weights2 + pattern3 %o% pattern3 + pattern4 %o% pattern4- 2*diag(10*10);


# weights3 = matrix (rep(0,100*100), 100, 100)
# weights3 = weights3 + pattern3 %o% pattern3 
# 		+ pattern5 %o% pattern5
# 		- 2*diag(10*10);


# iterate to see if this network really store the two images

run.hopfield (list(weights=weights2), pattern3,
	stepbystep = T, topo=c(10,10), replace = F)

run.hopfield (list(weights=weights2), pattern4,
	stepbystep = T, topo=c(10,10), replace = F)


# some more testings

run.hopfield (list(weights=weights2), pattern1,
	stepbystep = T, topo=c(10,10), replace = F)

run.hopfield (list(weights=weights2), pattern2,
	stepbystep = T, topo=c(10,10), replace = F)


# pattern in the middle 

# run.hopfield (list(weights=weights2), pattern5,
# 	stepbystep = T, topo=c(10,10), replace = F)


# run.hopfield (list(weights=weights2), pattern6,
# 	stepbystep = T, topo=c(10,10), replace = F)


# ex4

# weights = matrix (rep(0,100*100), 100, 100)
weights2 = weights2 + pattern3 %o% pattern3 - diag(10*10)


run.hopfield (list(weights=weights2), pattern3,
	stepbystep = T, topo=c(10,10), replace = F)


run.hopfield (list(weights=weights2), pattern4,
	stepbystep = T, topo=c(10,10), replace = F)


run.hopfield (list(weights=weights2), pattern2,
	stepbystep = T, topo=c(10,10), replace = F)


run.hopfield (list(weights=weights2), pattern1,
	stepbystep = T, topo=c(10,10), replace = F)


# ex5

weights2 = matrix (rep(0,100*100), 100, 100)
weights2 = weights2 + pattern3 %o% pattern3 + pattern4 %o% pattern4- 2*diag(10*10);
# weights = matrix (rep(0,100*100), 100, 100)
weights3 = weights2 - pattern3 %o% pattern3



run.hopfield (list(weights=weights3), pattern3,
	stepbystep = T, topo=c(10,10), replace = F)
