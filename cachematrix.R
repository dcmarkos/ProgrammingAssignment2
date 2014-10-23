## creates a list of 4 functions
## thus the functions are called with the $ operator attached to the list object
## note that the setinverse function is not called directly
## it is used by the cacheSolve function
## getinverse returns NuLL until after a direct call to cacheSolve with the object
## created by this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the matrix or
## returns the cached version stored in the object created by the above function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv 
}
