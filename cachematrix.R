## Put comments here that give an overall description of what your
## There are two functions defined in this file 
## That cache variable into environment

## makeCacheMatrix, sets the variable and saves to environment

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv 
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## CacheSolve, gets the result of matrix inverse, if it is cached then gets from memory
## otherwise computes and stores

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
