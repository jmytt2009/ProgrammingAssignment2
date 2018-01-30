## The functions in this file will compute the
## inverse of a square matrix and cache the
## solution.

## This function creates a special matrix object
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	    x <<- y
	    m <<- NULL
	}
	get <- function() x
	setmatrix <- function(mat) m <<- mat
	getmatrix <- function() m
	list(set = set, get = get,
	     setmatrix = setmatrix,
	     getmatrix = getmatrix)
}


## This function computes the inverse of a special matrix
## If the inverse has already been calculated, then return
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}
