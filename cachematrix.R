## Functions makeCacheMatrix and cacheSolve used together allow
## the caching of the inverse of a matrix. This caching avoids
## the repeated computation of the inverse, which can be time
## consuming.

## Function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
	z <- NULL
	set <- function(y) {
		x <<- y
		z <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) z <<- solve
	getInverse <- function() z
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)	
}

## Function cacheSolve computes the inverse of the special
## "matrix" object returned by the makeCacheMatrix function
## above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	z <- x$getInverse()
	if(!is.null(z)) {
		message("getting cached data")
		return(z)
	}
	data <- x$get()
	z <- solve(data, ...)
	x$setInverse(z)
	z
}
