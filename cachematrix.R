## Description: Create functions that will help cache the inverse of a matrix
## This will be achieved with 2 functions. 

## FunctionName: makeCacheMatrix
## Arguments: x is a matrix that is invertible (non-singular)
## Returns : list of functions 
##
## Description: makeCacheMatrix creates a list of functions that 
##  1. set - sets the matrix provided as argument 
##  2. get - gets the  matrix
##  3. setInverse - caches the provided inverse
##  4. getInverse - returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ##  1. set - sets the matrix provided as argument 
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    ##  2. get - gets the  matrix    
    get <- function() 
    {
        x
    }
    ##  3. setInverse - caches the provided inverse
    setInverse <- function(inverse) 
    {
        inv <<- inverse
    }
    ##  4. getInverse - returns the cached inverse    
    getInverse <- function()
    {
        inv
    }
    ## Creates a list of above functions and returns it
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## FunctionName: cacheSolve
## Arguments: x is a matrix that is of type makeCacheMatrix
## Returns : inverse of the provided matrix
##
## Description: cacheSolve returns the cached inverse of the matrix if available
##              if not calculates the inverse, caches it and returns the 
##              inverse of the matrix


cacheSolve <- function(x, ...) {
    
    ## Get the cached inverse
    inv <- x$getInverse()
    
    ## Check if cached inverse is valid
    if(!is.null(inv)) {
        message("getting cached data")
        # If valid, return cached inverse
        return(inv)
    }
    
    ## If not, get the matrix
    data <- x$get()
    
    ## Calculate the inverse of the matrix
    inv <- solve(data, ...)
    
    ## Cache the inverse of the matrix
    x$setInverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
