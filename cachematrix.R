## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ##prevent old inv being used
  ## set x to new value in new parent environment and reset inv to NULL in parent environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function (){x} #get function that retrieves x
  setInverse <- function(inverse) {inv <<- inverse} # set inv to new value 
  getInverse <- function() {inv} #retrieves inv
  ## return the above function to be used by cachesolve
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # retrieving stored inverse
  # check if iverse has been calculated, returns if there 
  if(!is.null(inv)){
    message("getting cached data")
  }
  
  #calculating, storing and returning inv if it's not already calculated
   mat <- x$get() 
   inv <- solve(mat, ...)
   x$setInverse (inv)
}
