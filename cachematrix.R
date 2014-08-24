## These functions are used to cache the inverse of supplied matrix, thus improving the
## the response time in computing the inverse for a matrix as 'inverse' computing can be
## a costly computation
##

## makeCacheMatrix is used to intialize the matrix and prepare for the caching

makeCacheMatrix <- function(x = matrix()) {
	#inverse is for holding the inverse of the current matrix
	inverse <- NULL
	
	#function to cache a matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	#returns the original matrix
	get <- function() x
	
	#sets the inverse for the cached matrix
	setinverse <- function(i) inverse <<- i

    #returns the cached inverse
    getinverse <- function() inverse
    
    #list with all the functions to cache and retrieve the matrix and its inverse
    list(set = set, get = get, serinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function is used to retrieve an inverse if it already exists for a matrix,
## otherwise it computes the inverse, caches and then returns it.

cacheSolve <- function(x, ...) {
	#First try to find if there is a cached inverse for the supplied matrix
	inv <- x$getinverse()
	if (!is.null(inv)) {
		#Found a cached inverse, so return it
		message("getting cached data")
		return(inv)
	}
	
	#inverse is not found in the cache, so start computing the inverse
	
	#First get the original matrix
	data <- x$get()
	
	#Use solve to get the inverse of the matrix
	inv <- solve(data)
	
	#store the computed inverse
	x$serinverse(inv)
	
	#return the computed inverse
	inv
}
