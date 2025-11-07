# Matrix Inversion Caching Functions
# This script implements two functions for caching matrix inversions:
# 1. makeCacheMatrix: Creates a special matrix object that can cache its inverse
# 2. cacheSolve: Computes the inverse of the special matrix, using cached values when available

#' Create a special matrix object that can cache its inverse
#'
#' This function returns a list of functions that can:
#' - set the matrix value
#' - get the matrix value
#' - set the inverse matrix
#' - get the inverse matrix
#'
#' @param x A square invertible matrix
#' @return A list of functions to interact with the matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse as NULL
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y       # Store the new matrix in the parent environment
    inv <<- NULL  # Reset the cached inverse when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#' Compute the inverse of a special matrix created by makeCacheMatrix
#'
#' This function computes the inverse of the matrix. However, if the inverse has
#' already been calculated (and the matrix has not changed), then it retrieves
#' the inverse from the cache to avoid redundant computation.
#'
#' @param x A special matrix object created with makeCacheMatrix
#' @return The inverse of the matrix
cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already calculated, return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix and calculate its inverse
  data <- x$get()
  
  # Calculate the inverse using solve()
  inv <- solve(data, ...)
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the inverse
  return(inv)
}

# Example usage:
# m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)  # Calculates and returns inverse
# cacheSolve(cm)  # Returns cached inverse
