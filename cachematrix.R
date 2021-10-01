## Functions that cache the inverse of matrix
### USE: To pass the result of makeCacheMatrix to cacheSolve
## Function set the matrix and the inverse in the environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function(){inv}
        list(set = set, get = get, setInverse = setInverse,
             getInverse - getInverse)
}


## This will compute and cache the inverse of the matrix
## x is the result of previous makeCacheMatrix
## the "..." is for additional arguments that can be made
##              to solve the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
        
