### function initialize perceptron:
### set weights, bias and activation function
# input:
#	dim: the dimensionality of the datapoints
#	activation: the desired activation function
# output:
#	model:  list(weights, bias, activation)
initializePerceptron <- function(dim, activation) {
	weights	<- runif(dim, min=-0, max=0)
	bias		<- runif(1, min=-0, max=0)
	model 	<- list(weights=weights, bias=bias, act=activation)
	return(model)
}	

### function train perceptron: 
### iterate over a dataset and learn online
# input:
#	model:  list(weights, bias, activation)
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
#	maxit: the number of iterations for training
#	learnRate: the learning rate for training
# 	stepbystep: wait for user to continue and print each step
# output:
#	model:  list(weights, bias, activation)	
#	error: the error over the entire data of the model after each training iteration
trainPerceptron <- function(model, x, y, maxit = 5, 
	learnRate = 0.1, stepbystep=T) {
	errors	<- rep(0, maxit)
	
	for (i in 1:maxit) {
		if (stepbystep) {
			readline()
			cat('iteration ',i,'\n')
		}
		# pick example:
		id <- id <- (i-1) %% dim(x)[1] + 1
#		cat('id',id,'\n')
		input	<- x[id,]
		targ	<- y[id,]
		model <- updatePerceptron(model, input, targ, learnRate, stepbystep)	
		if(dim(x)[2]<3){plotModel(x,y,id, model)}
		errors[i] <- computeError(model, x, y)
	}	
	return(list(model=model,errors=errors))
}
### function update perceptron:
### apply a single update to the model
# input:
#	model: list(weights, bias, act)
#	input: a datapoint (vector)
#	target: the corresponding target
# 	learnRate: the learning rate (real number)
# 	verbose: whether the results are to be printed
# output:
# 	model: the updated model, list(weights, bias, act) 
updatePerceptron <- function (model, input, targ, learnRate, verbose = TRUE) {
	# make prediction
	## TODO ##: make proper prediction
	pred	<- 0	

	# update weights and bias
	## TODO ##: write update rules for weights and bias
	model$weights <- model$weights ## + ...
	model$bias	<- model$bias ## + ...

	if (verbose) {

		cat('\tinput: ', input, '\n');
		cat('\ttarget: ', targ, '\n');
		cat('\tprediction: ', pred, '\n');
		cat('\tweights: ', model$weights, '\n');
		cat('\tbias: ', model$bias, '\n\n\n');
	}
	return(model)
}

### sigmoid function 
# input: x an array
# output: y = 1/ (1 + exp(-x))
sigmoid <- function(x, name=F) {
	if(name) {return("Threshold Binary")} # Ignore this line
	else {return(1 / (1 + exp(-x)))}
}

### threshold binary function 
# input: x an array
# output: y = ifelse(x > 0, 1, 0)
thresh.bin <- function(x, name=F) {
	if(name) {return("Threshold Binary")} # Ignore this line
	else {return((x > 0) * 1.)}
}


### function compute error:
### compute the error of the model wrt the dataset
# input: 
#	model: list(weights, bias, activation)
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
# output: classification error
computeError <- function(model, x, y) {
	nCases <- dim(x)[1]
	err	<- sum((y - model$act(x %*% model$weights + model$bias))^2)/nCases
	return(err)
}

### function plotModel
### plot the dataset: display the current example in black,
### positive examples in red, and negative examples in blue
### also plot the decision boundary of the current model
# input:
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
#	id: the index of the current datapoint
#	model: list(weights, bias, activation)
plotModel <- function(x,y,id, model) {
	d1min <- min(x[,1]) - 2; d1max <- max(x[,1]) + 2
	d2min <- min(x[,2]) - 2; d2max <- max(x[,2]) + 2
	plot(x[y==0,1], x[y==0,2], col='blue', 
		xlim = c(d1min,d1max), ylim = c(d2min,d2max),
		xlab = colnames(x)[1], ylab = colnames(x)[2])
	points(x[y==1,1], x[y==1,2], col='red')
	points(x[id,1], x[id,2], col='black')

	d1 <- seq(d1min, d1max, by=0.1)
	d2 <- (-model$bias - d1*model$weights[1]) / model$weights[2];
	lines(d1, d2, col='green')
}