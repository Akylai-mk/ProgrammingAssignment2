## Put comments here that give an overall description of what your
## functions do
## These functions set matrix and help to get its inverse
## Write a short comment describing this function
##This function is setting and getting a value of matrix

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL                             ## initialize m is NULL; will hold value of inverse 
  set <- function(y) {                  ## define the set function for matrix
    x<<-y                               ## value of matrix in parent environment
    m<<-NULL
  }
  get <- function() x                   ## get function will return value of the matrix argument
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Write a short comment describing this function
## This function is getting an inverse of a function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {                   ## if inverse has already been calculated
    message("getting cached data")     
    return(m)                          ## inverse should be retrieved from cache
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
