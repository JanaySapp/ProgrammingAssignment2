# Pair of functions that cache the inverse of a matrix.
# WARNING: It's assumed that the matrix supplied is 
# always invertible.
## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list (set = set, get = get,
              setinv = setinv,
              getinv = getinv
              )
}
## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- m(data, ...)
        x$setinv(m)
        m
}
