## These functions create an object that allows you to store a
## matrix and cache its inverse alongside it.

## Returns a list with functions to set and get a matrix and
## its inverse. Storing the inverse can be used to cache that value.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() {
		return(x)
	}

	setInverse <- function(inv) {
		i <<- inv
	}

	getInverse <- function() {
		return(i)
	}

	return(
			list(
					set = set,
					get = get,
					setInverse = setInverse,
					getInverse = getInverse
					))
}


## Takes a CacheMatrix as input and returns its inverse.
## If the inverse has already been cached, it will return
## the cached value, otherwise it will calculate it and
## cache it.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        return(i)
}