## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  require("MASS")
  inverse <- NULL
  set <- function(y) #set the value of the matrix{
    x <<- y
    inverse <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinverse <- function(ginv) inverse <<- ginv #set the value of the matrix inverse
  getinverse <- function() inverse              #get the value of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  require("MASS") # This package is Required to make the calculations
  -        inverse <- x$setinverse
  +        inverse <- x$getinverse() ## get the inverse if it has been calculated before
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  -        inverse <- ginv(data)
  +        inverse <- ginv(data,...)
  x$setinverse
  inverse
}
