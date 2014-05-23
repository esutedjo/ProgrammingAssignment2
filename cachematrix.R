## Two functions makeCacheMatrix and cacheSolve to calculate the inverse
## of matrix x with caching. Matrix x is assumed to be invertible

## Creates list z of functions to cache the inverse in matrix i

makeCacheMatrix <- function(x = matrix()) {
	i <<- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	z <<- list(set = set, get = get,
	           setinverse = setinverse,
	           getinverse = getinverse)
}


## Computes the inverse of matrix x. If the inverse has been calculated
## earlier (i is not NULL), then the inverse is returned from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- z$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- z$get()
	i <- solve(data)
	z$setinverse(i)
	i
}
