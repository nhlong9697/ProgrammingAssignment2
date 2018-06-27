## Caching the inverse of a matrix:
## Taking the inverse of a matrix is typically a fast operation.
## However, for a big matrix, it may take too long to compute the inverse especially if it is used in a loop.
## If the contents of a matrix are not changing, it is reasonable to cache the value of the inverse matrix for repeated use instead of recomputing.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function ()
    x
  setinverse <- function(inverse)
    inv <<- inverse
  getinverse <- function()
    inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function calculates the inverse of the special "matrix" created by makeCacheMatrix above.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the compuatation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
