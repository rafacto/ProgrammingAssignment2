## Functions that calculate and cache a inverse matrix. The goal is to 
## avoid calculating the inverse of a matrix (which is an expensive task)
## all the time is needed. 

## Receaves a inverseble matrix and store its inverse matrix. It contains a list of four functions:
## (1) sets the inversible matrix; (2) gets the inversible matrix; (3) sets the inverse matrix of the inversible matrix; and 
## (4) gets the inverse matrix of the inversible matrix
makeCacheMatrix <- function(x = matrix()) {
  invmtr <- NULL
  ## (1) sets the inversible matrix
  set <- function(y){
    x <<- y
    invmtr <<- NULL
  }
  ## (2) gets the inversible matrix
  get <- function() x
  ## (3) sets the inverse matrix of the inversible matrix
  setinvmtr <- function(y) invmtr <<- y
  ## (4) gets the inverse matrix of the inversible matrix
  getinvmtr <- function() invmtr
  
  ## The function returns a list containing the four functions created.
  list(set = set, get = get, setinvmtr = setinvmtr, getinvmtr = getinvmtr)
}


## Computes the inverse matrix of the inversible matrix created with the makeCacheMatrix function, but only if it have not 
#  been calculated yet. If it had already been calculated, it skips the computation and gets the inverse matrix from the cache. 
cacheSolve <- function(x, ...) {
  m <- x$getinvmtr()
  if(!is.null(m)){ #checks if the inverse matrix had already been calculated
    message("getting cached inverse matrix")  
    return(m) # returns the cached inverse matrix, finishig the function execution and avoiding recalculation
  }
  # if the inverse matrix is null (it have not been calculated before), the inversible matrix is get...
  data <- x$get()
  # ...its inverse matrix is calculated...
  m <- solve(data, ...)
  # ...and stored in makeCacheMatrix
  x$setinvmtr(m)
  m
}
