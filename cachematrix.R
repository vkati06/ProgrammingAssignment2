## These functions cache the inverse of a matrix instead of
## computing it repeatedly.


## This function creates a list of functions to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
        inv <- matrix(nrow = nrow(x), ncol = ncol(x))
        set <- function(y) {
                x <<- y
                inv <<- matrix(nrow = nrow(x), ncol = ncol(x))
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.na(inv[1,1])) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
