## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Get Matrix "m" from Cache
  m <- x$getinv()             
  ## Check if not null, Retrun value read from Cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if "m" is NULL, get origal Matirx
  mydata <- x$get()
  ## Calculate inverse using Solve
  m <- solve(mydata, ...)
  x$setinv(m)
  ## Returm inverse
  m
  
  
}
