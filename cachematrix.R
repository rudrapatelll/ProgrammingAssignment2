## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
        
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() 
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  if (nrow(mat) != ncol(mat)) {
    stop("The matrix must be square to compute its inverse.")
  }
  inv <- solve(mat, ...)  
  x$setInverse(inv)  
  
  inv  
}
