## Functions that calculate and cache a inverse matrix. The goal is to 
## avoid calculating the inverse of a matrix (which is an expensive task)
## all the time is needed. 

## Receaves a inverseble matrix and store its inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  invmtr <- NULL
  set <- function(y){
    x <<- y
    invmtr <<- NULL
  }
  get <- function() x
  setinvmtr <- function(y) invmtr <<- y
  getinvmtr <- function() invmtr
  list(set = set, get = get, setinvmtr = setinvmtr, getinvmtr = getinvmtr)
}


## Computes the inverse matrix only if it had not been calculated

cacheSolve <- function(x, ...) {
  m <- x$getinvmtr()
  if(!is.null(m))
    return(m)
  
  m <- solve(x$get())
  x$setinvmtr(m)
  m
}
