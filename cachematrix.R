## These functions create special inverse cached matrix object
## for easy retrieval of inverse data and return matrix inverse 
## using this object

## This function creates the special matrix object that can cache
## its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  # store the matrix locally and clear cache when set
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return the matrix
  get <- function() x
  
  # cache the matrix inverse
  setinverse <- function(inverse) m <<- inverse
  
  # return the cached inverse matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function calculates and caches the inverse using the cache matrix 
# If already calculated, it retrives and returns the cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # handle the case when cache is already available
  # just return the cache value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if cache is not avaiable calculate and cache the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
