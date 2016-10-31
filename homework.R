# Shuai Wang 11108339

# Some useful links
# http://mathesaurus.sourceforge.net/r-numpy.html

# ----------- some tests and remindings -------
# A = matrix(c(3,-4,1,2,2,0),2,3)

# D = matrix(rep(0,15), 3,5)

# diag(1,4)

# addOne <- function (x) {
# 	return (x + 1)
# }

# plot(x, y, type = 'l', col = 'blue')

# savePlot('sin.png', type='png')
# --------------------------------------------


# exercise 1:
a = c(1.5, -1, 3)
b = rep (1,3)
c = seq(2,24, by = 2)

euclidianLength = function (vector) {
	sum = 0
	for (e in vector){
		sum = sum +  e * e
	}
	return (sqrt(sum))
}


euclidianLength(a)
euclidianLength(b)
euclidianLength(c)

# next we prove the |a * v| =  a* |v| (hypothesis) by giving some examples:
alist = 1:10
result = matrix (0, 2, 10)
flag = TRUE
for (i in alist){
	# calculate |a * v|
	print (i)
	v =  runif (10)
	av = i * v 
	result[1,i] = euclidianLength(av)
	result[2,i] = i * euclidianLength(v)
	# print (result[1][i] )
	# print (result[2][i] )
	if (result[1,i] == result[2,i]){
		print('the result matches for this i')
	} else {
		flag = FALSE
	}
}

print ('The matrix is as follows')
print (result)
if (flag){
	print ('the matrix shows that our hypothesis is wrong')
	}else {print ('the matrix shows that our hypothesis is correct')}


# exercise 2:
v = c(0.5, 0.5)
w = c(-0.1, 0.4)

plot( c(-1,5), c(-1,5), col = 'white')
arrows(0,0,v[1], v[2], col = 'purple')
arrows(0,0,w[1], w[2])
v5 = 5 * v
arrows(0,0,v5[1], v5[2], col ='red')
vw = v + w
arrows(0,0,vw[1], vw[2], col ='blue')


# A = matrix (c(-1, 4, 2, 1) , 2, 2)
# B = matrix (c(4,3,1,5), 2,2)
# understand the difference between A * B and A %*% B

# exercise 3
# they are not necessarily the equal. 
# the following is a counter example

A = matrix (c(-1, 4, 2, 1) , 2, 2)
B = matrix (c(4,3,1,5), 2,2)

# (A %*% B) is not equal to (B %*% A) 
# the order of matrix multiplication is important

# exercise 4
phi = 45 / 180 * pi
Rt = matrix(c(cos(phi), sin(phi), -1* sin(phi), cos(phi)), 2, 2)

vr = Rt %*% v
arrows(0,0, vr[1], vr[2], col = 'green')


F = matrix(c(-1, 4, 2, 1), 2, 2)

det(F) # = -9

B = matrix(c(-1, 4, 2,1), 2,2)

# eigen(B)
# $values
# [1] -3  3

# $vectors
#            [,1]       [,2]
# [1,] -0.7071068 -0.4472136
# [2,]  0.7071068 -0.8944272
