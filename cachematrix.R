# Functions to cache the inverse of a matrix

# Usage
# Create a matrix
# B = matrix(c(1, 2, 3, 4, 5, 6, 7, 8,-7), nrow=3, ncol=3)
# Assign the result to a variable
# z <- makeCacheMatrix(B)
# Pass the variable to cacheSolve
# Note: We must pass makeCacheMatrix to the variables to ensure we inherit all of the 
#   functions from makeCacheMatrix
# Note: Passing the matrix itself won't help as none of the functions defined in 
#   makeCacheMatrix will be usable in cacheSolve
# cacheSolve(z)
# Return the cached value rather than recalculating
# cacheSolve(z)

## makeCacheMatrix takes a sets up a 'special' matrix with four functions which allow 
## it's inverse to be cached
makeCacheMatrix <- function(x = matrix()) {
  
  # make sure m is clean and tidy
  m <- NULL
  
  # set - if you need to update the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return the matrix
  get <- function() x
  
  # Store the value
  setmatrix <- function(solve) m <<- solve
  # Return it
  getmatrix <- function() m
  
  # Store the defined functions
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve uses the functions created in makeCacheMatrix to either:
## 1. Invert a matrix
## 2. return a previously cached inverse matrix

cacheSolve <- function(x=matrix(), ...) {
  
  # Set m to the matrix from makeCacheMatrix
  m <- x$getmatrix()
  
  # If the matrix is cached?
  if (!is.null (m) ) {
    
    # Return it and say so
    message("getting cached data")
    return(m)
  }
  
  # Otherwise get the matrix by calling get from makeCacheMatrix
  matrix <- x$get()
  
  # run solve against the matrix previously collected
  m <- solve(matrix, ...)
  
  # Set the matrix by calling set from makeCacheMatrix
  x$setmatrix(m)
  
  #Return the inverted (or chached) matrix
  m
}