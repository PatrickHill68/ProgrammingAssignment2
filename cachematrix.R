## makeCacheMatrix() wraps a matrix as an object that can cache its inverse.
## cacheSolve() returns the inverse of a matrix that has been encapuslated with makeCacheMatrix().

## Return an object that encapsulates a matrix with functions to access the matrix and its inverse.
## 		x is the matrix to be encapsulated

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setInverse <- function(inv) i <<- inv

	getInverse <- function() i

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of the the given matrix that has been wrapped with makeCacheMatrix.
## If a cached inverse value is available, it is returned. Otherwise, the inverse is computed, cached and returned.
## 		x is the wrapped matrix to be inverted.
cacheSolve <- function(x, ...) {

	i <- x$getInverse()

	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data = x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i

}
