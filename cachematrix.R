## These functions can create a matrix object, calculate matrix inverse,
## Check if a certain inverse is already in the cache, and calculate
## the matrix inverse if it cannot be found in the cache.


## makeCacheMatrix function can:
##1. Create/set a matrix object
##2. Get the matrix object
##3. Calculate/set the matrix inverse
##4. Get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_m <<- solve
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function can:
##1. Check if the inverse matrix is already available
##2. If it is not available, it will calculate the inverse matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        } else {
                data <- x$get()
                inv_m <- solve(data, ...)
                x$setinverse(inv_m)
                return(inv_m)
        }
}
