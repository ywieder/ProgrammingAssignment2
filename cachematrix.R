## the functions below create a matrix-like object that can cache its inverse
## this can save computational time, since matrix inversion can take a long time
## functions do

## creates the matrix-like object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  set(x)
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x)
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## returns the inverse of the matrix x
## if the inverse has not been cached, caches the inverse
## otherwise, returns the inverse from the cache, instead of computing it

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached inverse")
            return (inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

