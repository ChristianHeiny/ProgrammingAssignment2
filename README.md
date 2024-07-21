### Introduction

This second programming assignment will require you to write an R
function that is able to cache potentially time-consuming computations.
For example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. In this
Programming Assignment you will take advantage of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

### Example: Caching the Mean of a Vector

In this example we introduce the `<<-` operator which can be used to
assign a value to an object in an environment that is different from the
current environment. Below are two functions that are used to create a
special object that stores a numeric vector and caches its mean.

The first function, `makeVector` creates a special "vector", which is
really a list containing a function to

1.  set the value of the vector
2.  get the value of the vector
3.  set the value of the mean
4.  get the value of the mean

<!-- -->

    makeVector <- function(x = numeric()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmean <- function(mean) m <<- mean
            getmean <- function() m
            list(set = set, get = get,
                 setmean = setmean,
                 getmean = getmean)
    }

The following function calculates the mean of the special "vector"
created with the above function. However, it first checks to see if the
mean has already been calculated. If so, it `get`s the mean from the
cache and skips the computation. Otherwise, it calculates the mean of
the data and sets the value of the mean in the cache via the `setmean`
function.

    cachemean <- function(x, ...) {
            m <- x$getmean()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- mean(data, ...)
            x$setmean(m)
            m
    }

### Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

Write the following functions:

1.  `makeCacheMatrix`: This function creates a special "matrix" object
    that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.

For this assignment, assume that the matrix supplied is always
invertible.

### Assignment Begins Here

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the inverse matrix (cache)
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y  # Assign 'y' to 'x'; '<<-' assigns in the parent environment
    inverse <<- NULL  # Invalidate the cached inverse
  }
  
  # Function to get the matrix
  get <- function() {
    x  # Return 'x'
  }
  
  # Function to compute the inverse of the matrix (cacheable version)
  cacheSolve <- function(...) {
    # Check if the cached inverse is up-to-date
    if (!is.null(inverse)) {
      message("Getting cached data")
      return(inverse)
    }
    
    # If not, compute the inverse
    mat <- get()
    inverse <- solve(mat, ...)  # Compute inverse using solve()
    
    # Cache the inverse
    setInverse(inverse)
    
    # Return the computed inverse
    inverse
  }
  
  # Function to set the cached inverse
  setInverse <- function(inv) {
    inverse <<- inv  # Assign 'inv' to 'inverse'; '<<-' assigns in the parent environment
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheSolve = cacheSolve)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(cacheMatrix, ...) {
  cacheMatrix$cacheSolve(...)  # Use the cacheSolve function from makeCacheMatrix
}

### Example Usage

# Create a cacheable matrix object
cacheMat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Get the original matrix
cacheMat$get()
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
cacheMat$set(matrix(c(1, 0, 0, 1), nrow = 2))

# Compute and cache the inverse for the new matrix
cacheSolve(cacheMat)
# Output:
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1



  
  
