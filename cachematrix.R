## A set of functions to create a matrix that caches the inverse
##  and that calculate the inverse

## Creates a list to hold the matrix data and functions
##  to allow the inverse to be cached
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solves the inverse of the matrix, using the cached inverse if already calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

# testing:
# cached <- makeCacheMatrix(matrix(c(0,1,2,1,0,3,4,-3,8), nrow=3,ncol=3))
# cacheSolve(cached)
# cacheSolve(cached)
