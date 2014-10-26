## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse ;)
  
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then the cachesolve should retrieve the inverse from the cache.
  
  m <- x$getinv() ## Get matrix from cache            
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## if not null, return value from cache
  }
  ## else get original Matrix
  data <- x$get()
  ## calculate inverse with solve function
  m <- solve(data, ...)
  x$setinv(m) ## set the cache
  m ## and go!
  
}
