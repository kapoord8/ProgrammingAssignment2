## This function calculates the inverse of a matrix, and reduces the
## computing time by caching the result in case we need to use it again. 

## The follwong function sets the value of the special matrix before 
## getting it. Then it does the same for the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function computes the inverse of the matrix we computed 
## using the previous function. It checks first whether the inverse is 
## inverse computed and if so, it gets the inverse from cache. Either way
## it sets the new inverse to cache using setInverse()

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  new_inv <- x$get()
  inv <- solve(new_inv, ...)
  x$setInverse(inv)
  inv
}