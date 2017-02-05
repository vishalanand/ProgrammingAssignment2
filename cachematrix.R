## Put comments here that give an overall description of what your
## functions do
## 
## Example:
## mat <- makeCacheMatrix(diag(3))  # Create special matrix
## mat$get()                        # Get the matrix
## mat$getInverse()                 # NULL
## cacheSolve(mat)                  # inverse matrix
## cacheSolve(mat)                  # inverse matrix with message: "getting cached data"
## mat$getInverse()                 # inverse matrix

## Write a short comment describing this function
## makeCacheMatrix:
## Creates a special matrix to enable caching. 
## The input is a variable of type matrix.
## The function returns the following four functions as a list
##  * set matrix
##  * get matrix
##  * set inverse
##  * get inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Inverse value initially set to NULL
  cacheEval <- NULL
  
  # set function sets the matrix, not the inverse
  set <- function(matrixVal) {
    x <<- matrixVal
    cacheEval <<- NULL
  }
  
  # get function gets the matrix, but not the inverse
  get <- function() x
  
  # Manually set the inverse value
  setInverse <- function(inverseVal) cacheEval <<- inverseVal
  
  # getInverse() returns the inverse value
  getInverse <- function() cacheEval
  
  # Box the functions in a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve:
## Post creating the matrix, cacheSolve() computes the inverse and caches the result
## If cacheSolve() is reused on the same matrix, the prior result is returned, thus
## avoiding rework. A message is flashed when the pre-computed inverse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the current state of the inverse to see if it has been pre-computed
  cacheEval <- x$getInverse()
  
  # If inverse is pre-computed
  if(!is.null(cacheEval)) {
    # Return pre-computed inverse
    message("getting cached data")
    return(cacheEval)
  }
  
  # If inverse is not pre-computed
  # The matrix
  data <- x$get()
  
  # Solve for inverse (per the instructions, matrix is assumed invertible)
  cacheEval <- solve(data)
  
  # Cache the inverse
  x$setInverse(cacheEval)
  
  # Return newly computed inverse
  cacheEval
}
