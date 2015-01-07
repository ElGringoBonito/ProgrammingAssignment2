## These two functions create a matrix object whose inverse can
## be cached, then calculate and cache the inverse (unless the
## inverse has already been cached).

## makeCacheMatrix defines the ways to set and retrieve a matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {i <<- inverse}
        getinverse <- function() {i}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve first checks to see if the inverse of a matrix has
## been calculated. If it already has, it retrieves the inverse
## from the cache. If not, it calculates the inverse and caches
## it for future retrieval.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
