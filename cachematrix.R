## Put comments here that give an overall description of what your
## functions do
## This function solves the inverse of a matrix and then caches it.

## Write a short comment describing this function
## This function makes a cached copy of a matrix.

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the inverse property to NULL.
	i <- NULL

	## Method to set the matrix
	set <- function(matrix) {
		x <<- matrix
		i <<- NULL
	}

	## Method to get the matrix
	get <- function() x

	## Method to set the inverse of the matrix
	setInverse <- function(inverse) i <<- inverse

	## Method to get the inverse of the matrix
	getInverse <- function() i

	## Return a list of the methods used
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## This function calculates the inverse of a matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	xInverse <- x$getInverse()

	## Just return the inverse if its already set
	if(!is.null(xInverse)) {
		message("getting cached matrix inverse data")
		return(xInverse)
	}

	## Get the matrix from our object
	data <- x$get()

	## Calculate the inverse using matrix multiplication
	xInverse <- solve(data) %*% data

	## Set the inverse to the object
	x$setInverse(xInverse)

	## Return the matrix
	xInverse
}

