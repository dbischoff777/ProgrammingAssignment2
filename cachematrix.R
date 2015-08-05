## The following functions will try to cache and compute the 
## inverse of a matrix.

## This function creates a special matrix object
## that can cache its inverse.

createCacheMatrix <- function(cmtx = matrix()) {
  inverse <- NULL
    set <- function(x) {
      cmtx <<- x;
      inverse <<- NULL;
    }
    
  get <- function() return(cmtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This will compute the inverse of the special matrix
## returned by createCacheMatrix from above. If the inverse has
## already been calculated, then cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(cmtx, ...) {
  inverse <- cmtx$getinv()
    if(!is.null(inverse)) {
     message("Trying to get cached data...")
      return(inverse)
    }
  
  data <- cmtx$get()
  invserse <- solve(data, ...)
  cmtx$setinv(inverse)
  return(inverse)
}
