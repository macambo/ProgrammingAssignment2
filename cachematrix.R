## These two functions will cache the inverse of a matrix to avoid the costly computation
## of having to compute the inverse multiple times. 

## This first function will create a special "matrix" that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) inverse <<- inverse
    getinverse <- function() inverse
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This second function will compute the inverse of the special matrix returned from the
## makeCacheMatrix function. If the inverse has already been calculated and the matrix has
## not changed then the function will retrieve the inverse that was cached in the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached result")
    return(inverse)
  }
  
  dat <- x$get()
  inverse <- solve(dat, ...)
  x$setinverse(inverse)
  return(inverse)
}

##Testing the functions to make sure they work as intended 

mdat <- matrix(c(2, 3, 2, 2), nrow = 2, ncol = 2, byrow = TRUE)
m1 <- makeCacheMatrix(mdat)
cacheSolve(m1)
