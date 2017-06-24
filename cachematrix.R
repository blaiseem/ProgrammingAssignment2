## These functions allow the user to cache the inverse of a square matrix
## to save in computation time in not having to recalculate the inverse of
## a square matrix

## makeCacheMatrix creates a list of functions to set and get a matrix, and set
## and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get matrix
    get <- function() x
    
    ## set matrix inverse
    setinverse <- function() m <<- solve(x)
    
    ## get matrix inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve returns the inverse of a square matrix by first checking if it
## already exists in the cache and using this object
## If not the inverse is calculated

cacheSolve <- function(x, ...) {
    ## set m to cached inverse
    m <- x$getinverse()
    
    #if nothing in cache, then compute inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse()
    m
        ## Return a matrix that is the inverse of 'x'
}
