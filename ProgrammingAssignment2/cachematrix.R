## This pair of functions caches the inverse of a matrix to avoid repetitive, 
## time-consuming calculations (assumes matrix supplied is always invertible)

## makeCacheMatrix: Creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Returns a list of functions that allows user to input and retrieve
    ## information in the cache
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function()i
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse is already been calculated (and the 
## matrix has not changed), the cacheSolve will retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Attempts to retrieve inverse from the cache. If it exists (i.e. is
    ## not NULL), the inverse is returned and the function ends
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    ## If the inverse does not already exist, calculate the inverse and 
    ## inputs it into the cache
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
