## Functions to solve for the inverse of a square matrix, and cache the result for reuse

## create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set: store the given matrix along with variable m to store inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get: just return matrix x
  get <- function() x
  ## setmatrix: cache inverse for later reuse
  setmatrix <- function(solve) m <<- solve
  ## getmatrix: return cached inverse of x
  getmatrix <- function() m
  ## return list of above functions for use by cacheSolve
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## calculate the inverse of a given matrix, using cached solution if available
cacheSolve <- function(x=matrix(), ...) {
  ## get matrix along with cached inverse, if available
  m <- x$getmatrix()
  ## if cached inverse exists, return that
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## otherwise, get matrix data and calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  ## cache the inverse for later reuse
  x$setmatrix(m)
  ## return the matrix's inverse
  m
}
