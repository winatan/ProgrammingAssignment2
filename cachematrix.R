## Functions that cache the inverse of a matrix to reduce computational costs

## Creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL         ## Initialise as NULL, holder of matrix inverse
        set <- function(y){
                x <<- y     ##  value of matrix in parent environment
                inv <<- NULL  ## if new matrix, reset inv to NULL
        }
        get <- function() {x}
        
        setInverse <- function(inverse) {inv <<- inverse} ## assigns value of inv in parent env
        getInverse <- function() {inv}                    ## Gets value of inv when called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes inverse of the 'matrix' returned by makeCacheMatrix
## If the inverse is calculated, cacheSolve will retrieve from cache

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}