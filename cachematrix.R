## The functions written below aim at caching the inverse of a matrix, thus
## preventing the repeat of its (costly) computation afterwards.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## In particular, it creates a list containing a function which sets the value of
## the matrix itself, gets the matrix value, sets the inverse of the matrix
## and then gets it.

makeCacheMatrix <- function(x = matrix()) {
    i = NULL
	set<-function(y) {
        x <<- y
        i <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) i<<-solve
	getInverse <- function() i
	list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
	    	message("getting cached data")
	    	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
