library(RSNNS)

# function compute accuracy
# input: 
#	model: list(weights, bias)
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
# output: classification error
compute.accuracy <- function(model, x, y) {
	n.cases <- dim(x)[1]
	pred <- (predict(model, x) > 0.5) * 1.
	acc	<- sum((y == pred)*1.)/n.cases
	return(acc)
}

#############################################################


# set training variables
maxit 	        <- 100
learn.rate 	<- 0.1
size		<- c(5)


datafile 	<- "OR.dat"
#datafile 	<- "XOR.dat"
#datafile 	<- "leftright.dat"
ratio 	<- 10 #ntrain/ntest, 
			#determines proportion of the data used for evaluation
# load data, put the examples in vector x, and the class labels (0 or 1) in vector y
dat 		<- read.table(datafile, header = T) 
# randomize the order
rand <- sample(nrow(dat))
dat <- dat[rand,]

width 	<- dim(dat)[2]
x 		<- as.matrix(dat[,1:width-1])
y 		<- as.matrix(dat[,width])

# change arbitrary class names to 0/1:
if (! all(y %in% c(0,1))) {y <- as.matrix((y[,]==y[1,])*1)}


# split the dataset
ncases <- dim(x)[1]
if(ncases<10) {
	cat('Training data and test data are the same.\n')
	xtrain 	<- x
	ytrain 	<- y
	xtest 	<- x
	ytest 	<- y
} else {
	ntrain 	<- round(ncases * ratio / (ratio+1))
	xtrain 	<- x[1:ntrain,]
	ytrain 	<- as.matrix(y[1:ntrain,])
	xtest 	<- x[(ntrain+1):ncases,]
	ytest 	<- as.matrix(y[(ntrain+1):ncases,])
}

# initialize and train a perceptron
model <- mlp(xtrain, ytrain, size=size, maxit = maxit, learnFuncParams = learn.rate)
# report result
plotIterativeError(model)
cat("Train accuracy: ", compute.accuracy(model, xtrain, ytrain), "\n")
cat("Test accuracy: ", compute.accuracy(model, xtest, ytest), "\n")


