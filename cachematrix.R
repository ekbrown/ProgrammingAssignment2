## Script that defines two functions:
## makeCacheMatrix() provides getter and setter methods for a matrix and its inverse
## cacheSolve() calculates the inverse of the matrix supplied from makeCacheMatrix()

## A class that gets and sets a square matrix and gets and sets its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_matrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function() {
    x
  }
  set_inverse <- function(inverse) {
    inv <<- inverse
  }
  get_inverse <- function() {
    inv
  }
  output <- list(set_matrix = set_matrix, 
       get_matrix = get_matrix, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse
  )
  return(output)
} # end function definition

## Calculates the inverse of the matrix in the object created by makeCacheMatrix() or return the cached inverse
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  return(inv)
} # end function definition
