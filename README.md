## Function: makeCacheMatrix
## Description: Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix (cache)
  inverse <- NULL
  
  ## Function to set the matrix
  setMatrix <- function(y) {
    x <<- y  # Assign 'y' to 'x'; '<<-' assigns in the parent environment
    inverse <<- NULL  # Invalidate the cached inverse
  }
  
  ## Function to get the matrix
  getMatrix <- function() {
    x  # Return 'x'
  }
  
  ## Function to compute the inverse of the matrix (cacheable version)
  cacheSolve <- function(...) {
    ## Check if the cached inverse is up-to-date
    if (!is.null(inverse)) {
      message("Getting cached data")
      return(inverse)
    }
    
    ## If not, compute the inverse
    mat <- getMatrix()
    inverse <- solve(mat, ...)  # Compute inverse using solve()
    
    ## Cache the inverse
    setInverse(inverse)
    
    ## Return the computed inverse
    inverse
  }
  
  ## Function to set the cached inverse
  setInverse <- function(inv) {
    inverse <<- inv  # Assign 'inv' to 'inverse'; '<<-' assigns in the parent environment
  }
  
  ## Return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheSolve = cacheSolve)
}

## Function: cacheSolve
## Description: Compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(cacheMatrix, ...) {
  cacheMatrix$cacheSolve(...)  # Use the cacheSolve function from makeCacheMatrix
}

### Example Usage

# Create a cacheable matrix object
cacheMat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Get the original matrix
cacheMat$getMatrix()
# Output:
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# Compute and cache the inverse
cacheSolve(cacheMat)
# Output:
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Get the cached inverse
cacheMat$getInverse()
# Output:
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Now, if we try to compute the inverse again, it should retrieve the cached result
cacheSolve(cacheMat)
# Output:
# Getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Change the matrix
cacheMat$setMatrix(matrix(c(1, 0, 0, 1), nrow = 2))

# Compute and cache the inverse for the new matrix
cacheSolve(cacheMat)
# Output:
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1



  
  
