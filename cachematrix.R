## We use the function 'makeCacheMatrix' to create a special kind of matrix that can cache its own inverse

## 'cacheSolve' is the function which gives the inverse of the special matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ## this function is used to return the inverse of the created matrix
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

##we can use the above created function as follows
mt <- makeCacheMatrix(matrix(1:4, 2, 2))
mt$get()
mt$getInverse()
cacheSolve(mt)
mt$getInverse()
