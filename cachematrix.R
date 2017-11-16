## Put comments here that give an overall description of what your
## functions do

## The below functions are used to create a special function that stores a matrix and caches its inverse. 
## The first function, makeCacheMatrix creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The below function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
