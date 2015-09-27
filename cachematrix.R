## cachematrix.R
## save processing time by cacheing the inverse, don't recalc it if we've already done so.
## Usage:   
#     mtx <- cbind(c(1,3),c(2,4))
#     o <- makeCacheMatrix(mtx)
#     cacheSolve(o)

## This function encapsulates the mtx that you set it with, and
# caches its inverse, the first time you ask for the inverse.
# Its return val is the list-o-funcs that you use to get/set.

makeCacheMatrix <- function(mx = matrix()) {
  inv <- NULL # not cached yet
  set <- function(mxy) {   # why doesn't the print statement ever show?
    mx <<- mxy
    inv <<- NULL   # still not cached yet
  }
  get <- function() mx
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # we return this list of access funcs.  And yet, why never set?
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of the matrix.  If it's already
## been cached, return that, else calc and cache the inv, 
## before returning it.
# 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the one stored in 'x' -
  # check first to see if we have a cached inverse.
  ret <- x$getinverse()
  if (!is.null(ret)) { 
    message("Returning cached object.")
    return(ret) 
  } 
  # else:
  
  ret <- solve(x$get())  # calc the inverse
  x$setinverse(ret)   # , cache it
  return(ret) # , and return it
}
