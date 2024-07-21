### makeCacheMatrix and CacheSolve Functions
## Note: Please be kind and constructive with your comments. I can do the same. Thank You!
## Function: makeCacheMatrix
## Description: Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y  # Assign 'y' to 'x'; '<<-' assigns in the parent environment
    inverse <<- NULL  # Invalidate the cached inverse
  }
  getMatrix <- function() {
    x  # Return 'x'
  }
## Function: cacheSolve
## Function to compute the inverse of the matrix (cacheable version)
  cacheSolve <- function(...) {
    ## Check if the cached inverse is up-to-date
    if (!is.null(inverse)) {
      message("Getting cached data")
      return(inverse)
    }
    mat <- getMatrix()
    inverse <- solve(mat, ...)
    setInverse(inverse)
    inverse
  }
  ## Function to set the cached inverse
  setInverse <- function(inv) {
    inverse <<- inv  # Assign 'inv' to 'inverse'; '<<-' assigns in the parent environment
  }
  ## Return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheSolve = cacheSolve)
}
## Description: Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(cacheMatrix, ...) {
  cacheMatrix$cacheSolve(...)  # Use the cacheSolve function from makeCacheMatrix
}
### Example Usage
cacheMat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))
cacheMat$getMatrix()
      [,1] [,2]
[1,]    1    3
[2,]    2    4
cacheSolve(cacheMat)
 Output:
     [,1] [,2]
 [1,]   -2  1.5
 [2,]    1 -0.5
cacheMat$getInverse()
Output:
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(cacheMat)
 Output:
Getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheMat$setMatrix(matrix(c(1, 0, 0, 1), nrow = 2))
cacheSolve(cacheMat)
 Output:
      [,1] [,2]
 [1,]    1    0
 [2,]    0    1



  
  
