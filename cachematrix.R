## Caching the inverse of a matrix
## makeCacheMatrix() creates an R object that stores a matrix and its inverse
## cacheSolve() requires an argument that is returned by makeCacheMatrix()
## to retrieve the inverse from the cached value

## The following function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  computeInverse <- function() {
    if (!is.null(j)) {
      message("getting cached data")
      return(j)
    }
    j <- solve(x)
    message("computing inverse")
    j
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       computeInverse = computeInverse)
}

## cacheSolve() retrieves the inverse of the matrix
## from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
  j
}
