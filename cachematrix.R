## Put comments here that give an overall description of what your
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

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
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
#It first checks if the inverse has already been computed. If so, it gets the result and skips the
#computation. If not, it computes the inverse, sets the value in the cache via setinverse function

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
