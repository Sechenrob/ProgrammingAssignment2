## --Put comments here that give an overall description of what your--
## --functions do--
## These functions should allow us to cache the inverse of a matrix
## so that repeated calls of our invert function will not re-execute solve()
## Instead of interacting directly with a matrix,
## we will just interact with the values of the functions produced by makeCacheMatrix

## --Write a short comment describing this function--
## This creates a 'vector' of functions related to caching the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv) 
}


## --Write a short comment describing this function--
## Return a matrix that is the inverse of 'x'
## If the inverse of 'x' is already defined, return the cached value.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinv(inv)
	inv
}
