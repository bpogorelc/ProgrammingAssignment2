## Because it is usually a costly to compute inverse of a matrix their may be
## beneficial to cache the inverse of a matrix rather than compute it repeatedly.
## The following functions are used to create a special object that stores a
## matrix and caches its inverse.

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL # assign inverse value to NULL
  
  # set value of the matrix. If the matrix has changed, reset inv to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
    
  get <- function() x # get value of the matrix
  
  set.inverse <- function(inverse) { # set inverse of the matrix
    inv <<- inverse
  }
   
  get.inverse <- function() inv # get inverse of the matrix
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## The function cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the function cacheSolve should get the inverse from
## the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$get.inverse()
  
  # If inverse exists, check if it is already cached and return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()  # if it does not exist, get the matrix
  
  inv <- solve(data, ...)  # compute inverse of the matrix
  
  x$set.inverse(inv)  # cache inverse of the matrix
  
  inv   # return inverse
}