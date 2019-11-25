## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
  set <- function(y) {
         m <<- y
          i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve computes the inverse of the special "matrix" in makeCacheMatrix above

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if (!is.null(i)) {
          message("Getting Cached Data")
          return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}
