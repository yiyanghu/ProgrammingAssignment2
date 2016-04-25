## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. Here we cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<-y
                i <<-NULL
        }
        get <- function() x
        setinverse <- function(given) inv <<-given
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <-solve(data)
        x$setinverse(inv)
        inv
}
