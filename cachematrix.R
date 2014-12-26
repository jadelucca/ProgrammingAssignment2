## These functions, makeCacheMatrix() and cacheSolve(), provide a matrix object
## with storage to cache its inverse and a matrix object representing the
## inverse, respectively. cacheSolve() uses the caching mechanism created by
## makeCacheMatrix() to cache the inverse when it is has not already been
## calculated, thereby eliminating the need to calculate it after the first 
## time.

## makeCacheMatrix() returns a set of functions to get and set a matrix and its
## inverse. The matrix it represents can be provided as an argument to the
## function. If no matrix is provided, an empty one is created. The inverse of
## the matrix is empty (NULL) until set using setInverse(). The matrix
## represented by the object this function returns can be changed using set(),
## in which case the cached inverse is set to NULL again

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() returns the inverse of a matrix. It first checks to see if the
## matrix specified already has a cached inverse, and if it does it returns it.
## If it doesn't, it calculates it, sets it for the specified matrix using
## setInverse(), and then returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
