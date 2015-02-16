##This code allows for computationally expensive calculations to be stored in cache for quick look up


## MakeCacheMatrix is a function that takes in a matrix and stores the matrix
## and its inverse in a new object.  It also allows creates 4 functions which can be called
## get, set, getInverse, and setInverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve looks for the inverse of the input in cache and if it exists returns it.
## if the inverse isn't in cache, it calculates it and then stores it in cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
