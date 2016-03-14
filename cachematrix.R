## The following function creates a matrix object which caches its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function computes the inverse of the matrix created by the function makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Fatched from cache.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Testing above functions 
##> m <- matrix(1:4, 2,2)
##> m
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cm <- makeCacheMatrix(m)
##> cm$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(cm)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(cm)
##Fatched from cache.
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 
