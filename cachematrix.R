## Below two functions implement caching functionality to inverse matrix
## First function makeCacheMatrix creates a matrix, inverse the matrix and cache it
## Whenever matrix inversion is required it is first checked in cache,
## if not available then only it is calculated in cacheSolve function



## makeCacheMatrix function accepts a matrix as its parameter 
## and returns a new object
## that has functions to set the matrix, get the matrix
## value, set the value of inverse matrix, get the value of
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
    inv <- NULL
  # set the value of the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  #get the value of the matrix
    get <- function() {
      x
    }
    #set the value of the inverse matrix
    setInverse <- function(inverse) {
      inv <<- inverse
    }
    # get the value of the inverse matrix
    getInverse <- function() {
      inv
    }
    
    # returns a list of all above functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
    
}


## This function does the operation to inverse the matrix created 
## in above function, and saves it in "cache" for it to
## be used again if matrix is not changed

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached/calculated,
  # if yes, fetch the value from there/cache
    inv <- x$getInverse()
  
    if(!is.null(inv)) {
    
      message("getting cached data")
    
      return(inv)
    }
  
  # else, first get the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(data, ...)
  
  # now cache the inverse of the matrix
  x$setInverse(inv)
  
  # Returns the inverse of the matrix
  inv
}
