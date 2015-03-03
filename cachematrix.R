## The functions below create a matrix 'x' and calculate its inverse, caching
## the resulting object. So the inverse of an input matrix is calculated only 
## once, which saves computation time.

## The function below creates a special matrix that contains a list of 
## four functions to get or set the matrix, and to get or set the inverse of
## this matrix.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
			x <<- y
			i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## This function checks if the inverse of the input matrix has already been 
## calculated. If so, it gets the inverse from the cache and returns it.
## Otherwise, it calculates the inverse and caches it for next calculations.

cacheSolve <- function(x, ...) {
		i <- x$getinverse()
		if(!is.null(i) {
			message("getting cached data")
			return(i)
		}
		data <- x$get()
		i <- solve(x, ...)
		x$setinverse(i)
		i
}