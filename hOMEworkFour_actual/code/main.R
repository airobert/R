source("toDoPerceptron.R")


# set training variables
maxit 	<- 15
learn.rate 	<- 0.1
act		<- thresh.bin
stepbystep	<- T


datafile 	<- "OR.dat"
#datafile 	<- "XOR.dat"
#datafile 	<- "leftright.dat"
ratio 	<- 10 #ntrain/ntest, 
			#determines proportion of the data used for evaluation

# load data, put the examples in vector x, and the class labels (0 or 1) in vector y
dat 		<- read.table(datafile, header = T) 
# randomize the order
rand 		<- sample(nrow(dat))
dat 		<- dat[rand,]

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
model		<- initializePerceptron(dim(x)[2],act) 
cat('model before training:\n')
cat('\tweights: ', model$weights, '\n');
cat('\tbias: ', model$bias, '\n');
cat('\tactivation function: ', act(0,T), '\n\n\n');




trained	<- trainPerceptron(model, xtrain, ytrain,maxit = maxit, learnRate=learn.rate, stepbystep=stepbystep)
model 	<- trained$model
trainErrors <- trained$errors

cat('model after training:\n')
cat('\tweights: ', model$weights, '\n');
cat('\tbias: ', model$bias, '\n\n\n');


# report training error
plot(1:maxit, trainErrors, type='l', xlab="iter", ylab="error")
cat("Train accuracy: ", 1-computeError(model, xtrain, ytrain), "\n")

# report accuracy on test data
cat("Test accuracy: ", 1-computeError(model, xtest, ytest), "\n")



