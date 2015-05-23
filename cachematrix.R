## The following two functions calculate the inverse of a matrix and cache the result.
## Caching is achieved by utilizing R’s lexical scoping environment.


## makeCacheMatrix creates a list of following four functions
## sets the matrix to passed in parameter matrix and the inverse value to null
## gets the matrix
## sets the inverse matrix to passed in inverse matrix parameter
## gets the inverse matrix

makeCacheMatrix <- function(xMat = matrix()) {
	inverse <- NULL
	set <- function(yMat) {
		xMat <<- yMat
		inverse <<- NULL
	}
	get <- function() xMat
	setInverse <- function(i) inverse <<- i
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # returns vector type of list of functions
}


## cacheSolve computes the inverse of a matrix if it does not already exist in the cache.
## Otherwise the function returns the cached value.

cacheSolve <- function(xMakeMatrix, ...) {
        iInverse <- xMakeMatrix$getInverse() # returns the Inverse of the special function vector
	
	# if an inverse exists in cached environment, return the value and leave the function
	if(!is.null(iInverse)){
		message("getting cached inverse matrix")
		return(iInverse)
	}
	# otherwise if inverse was not cached, compute the inverse and store in cache
	data <- xMakeMatrix$get()
	iInverse <- solve(data, ...)
	xMakeMatrix$setInverse(iInverse)
	return(iInverse)
}
