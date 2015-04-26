## This file contain functions to manipulte matrix
## Function results are cached, to speed up calculations,  
##
##
##
##
## Function, `makeCacheMatrix` creates a special "matrix" object, which is
## really a list containing a function to
## 
## 1.  set the value of the matrix (set)
## 2.  get the value of the matrix (get)
## 3.  set the value of the inverse matrix (setinverse)
## 4.  get the value of the inverse matrix (getinverse)


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}



## Function`cacheSolve` calculates the inverse matrix for the special "matrix" object
## created with the above function. It first checks to see if the
## inverse matrix mean has already been calculated. If so, it `get`s the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates inverse matrix and store result in cache via the `setinverse`
## function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}

