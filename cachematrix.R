## Caches the inverse of a matrix

## Example usage:

##  source("cachematrix.R");
##  mat <- makeCacheMatrix(matrix(c(c(14,13), c(15,41)), 2,2));
##  cacheSolve(mat) ## Should output "Inverse was set"
##  cacheSolve(mat) ## Should output "Found cached inverse"
##  cacheSolve(mat) ## Should output "Found cached inverse"

## Define a function that returns an cached matrix object
makeCacheMatrix <- function(x = matrix()) {

  ## Define a variable to hold cached inverse of matrix
  inverse <- NULL

  ## Define set-function for resetting the cached matrix
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }

  ## Define function to return cached matrix
  get <- function() x

  ## Define function for setting inversed matrix
  setinverse <- function(inv) inverse <<- inv

  ## Define function for getting inversed matrix
  getinverse <- function() inverse

  ## Return a list of functions to get/set object values
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return cached inverse of given matrix x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Check to see if the x matrix already has a cached inverse matrix

  ## If so, return cached inverse
  inv <- x$getinverse()
  if ( !is.null(inv)) {
    message("Found cached inverse")
    return(inv)
  }

  ## If not, get original matrix and create cache
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)

  message("Inverse was set")

  ## Then return the new cached inverse
  inv
}
