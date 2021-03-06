## Programming Assignment 2
## Caching the inverse of a matrix

## makeCacheMatrix function contains several functions to set and get the values of the 
## input/output

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(INV) inv <<- INV
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve function first checks if the inverse of the matrix has
## already been evaluated. If not then it computes it otherwise returns
## the previous one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
