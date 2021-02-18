## This assignment consists of 2 functions that form an inverse matrix cache
## so the inverse of the matrix doesn't need to be recalculated every time. 
## The makeCacheMatrix function constructs a list containing 4 functions within 
## the same environment. It is important that the functions of the list are in 
## the same environment in order to work properly. The second function 
## (cacheSolve) checks if there is a valid inverse matrix and if so this matrix 
## is returned. If the inverse is not available the new inverse matrix is calculated.

## Initially the list contains the data matrix and an inverse matrix variable is 
## set to NULL. This is set to NULL to initialise the firts calculation for an inverse matrix.
## Set() is used to change the datamatrix in the cache and reset the inverse matrix to NULL. 
## In turn the new inverse needs to be calculated by the cacheSolve function.
## get() returns the datamatrix 
## setInverse() recieves a the new inverse matrix values from cacheSolve().
## getInverse() returns the inverse matrix when asked by cacheSolve().
## The list command forms the names of the functions in the list. 
## This way they can be called from the list.


makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <- NULL
  }
  get <- function() x 
  setInverse <- function(inversematrix) invmat <<- inversematrix
  getInverse <- function() invmat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve needs the output list from makeCacheMatrix as an argument, otherwise it doesn't work.
## first the inversematrix is retrieved from the cache. The if statement checks if 
## there is an inverse matrix and returns it if there is one.
## Otherwise the function retrieves the data stored in the cache and calculates 
## a new inverse matrix, which is returned in the end.

cacheSolve <- function(x, ...) {
  invmat <- x$getInverse()
  
  if (!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data,...)
  x$setInverse(invmat)
  invmat
}
