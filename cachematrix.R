## cachematrix.R
## (Programming assignment 2)
## makeCacheMatrix creates a special "cacheMatrix", which is really a list containing a function to
## --set the value of matrix being chached
## --get the value of matrix being cached
## --set the value of inverse (setinverse)
## --get the value of inverse (getinverse)
##
## Create a cache marix object that can be used to
## obtain the inverse of the marix
## the inverse is calculated only once
##
## Description: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # set inverse variable as null
  matInverse <- NULL
  
  #definition of private set function
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  
  #definition of private set function
  get <- function() x
  
  #definition of private setinverse function
  setInverse <- function(givenInverse) matInverse <<- givenInverse
  
  #definition of private get inverse function
  getInverse <- function() matInverse
  
  # return a list for the "special" matrix with all these functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Description: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invFn <- x$getInverse()
  
  ## if it isnt null, get the cached data!
  if(!is.null(invFn)) {
    message("getting cached data")
    return(invFn)
  }
  
  data <- x$get()
  invFn <- solve(data, ...)
  
  ## now that we have the inverse, cache it, to avoid recalculations
  x$setInverse(invFn)
  invFn
}

## Sample usage and output:
## source("cacheMatrix.R")
##  myMat <- matrix(5:8, nrow=2, ncol=2)
## > myMat
##      [,1] [,2]
## [1,]    5    7
## [2,]    6    8
##  myCacheMat <- makeCacheMatrix(myMat)
## > cacheSolve(myCacheMat)
##     [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
##  We can then use get and set methods to change values:
##  -------------------------------------------------------
##  cacheMatrix$set(myMat)      # Change the matrix being cached.
##  myMat <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x
