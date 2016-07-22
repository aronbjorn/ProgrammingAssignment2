## the following functions are meant to check if an inverse of the matrix has already been calculated and
## "save" the costly computation of calculating an inverse if it has already been done

## makeCacheMatrix creates a "matrix" object that can  cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" that is returned from makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}