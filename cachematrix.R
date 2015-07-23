## this construction makes a matrix cachable

## give this function a matrix and it will cache it and its inverse matrix
## overwrite the matrix object with the $set function
makeCacheMatrix <- function(x = matrix()) {
  ## set inv (result of inversion calculation) to NULL at start
  inv <- NULL
  
  ## set inv (result of inversion calculation) to NULL if new matrix data is received
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## returns the matrix, used for calculation
  get <- function() x
  
  ## save the inverted data within a different environment
  setinv <- function(inverted) inv <<- inverted
  
  ## get the inverted data within the cache 
  getinv <- function() inv
  
  ## a list of function names
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## expects a special object "makeCacheMatrix" with invertible matrix
## CAUTION:
## if invertible matrix not supplied no error handling is applyed

cacheSolve <- function(x) {
  ## look out for cached data
  alreadyCalculated <- x$getinv()
  
  ## if there is calculated data, return it
  if(!is.null(alreadyCalculated)) {
    message("cache found - return cached inverse matrix")
    return(alreadyCalculated)
  }
  
  ## else calculate the matrix
  ## first get the matrix object, saved within the special function
  yetToCalculate <- x$get()
  
  ## calculate the inverse of the matrix
  ## CAUTION: we asume within this assignment that 
  ## the matrix is invertible, so no error handling is applyed
  inv <- solve(yetToCalculate)
  
  ## for later use save the result to cache
  x$setinv(inv)
  
  ## return the result of calculation
  inv
}
