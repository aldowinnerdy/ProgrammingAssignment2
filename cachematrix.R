## The combination of the two functions below will let us either call
## the inverse of a matrix if it was cached before or compute a new 
## inverse of a matrix

## makeCacheMatrix creates a list of four functions to set and get 
## the value of a matrix as well as to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## defining i, x is already defined as function argument
    i <- NULL
    
    ## set a new matrix, if you run set(), you are resetting the
    ## inverse to null in the parent environment
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get a matrix from parent environment
    get <- function() x
    
    ##set the inverse i to the parent environment
    setinv <- function(inv) i <<- inv
    
    ##get the inverse i from parent environment
    getinv <- function() i
    
    ##return value of the function is a list of 4 functions with names
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix or getting the 
## cached value. The first input argument of this function is the 
## list produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    
    ## if it is not NULL, means we found a cached data, and return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if it is NULL, we are getting a new inverse from the data
    data <- x$get()
    
    ## calculating the inverse
    i <- solve(data)
    
    ## cached the new inverse
    x$setinv(i)
    
    ## return the inverse
    i
}
