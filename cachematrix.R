## These 2 functions show how the operand "<<-" can be used to change the scope of variables.

## makeCacheMatrix is a function that caches a matrix inverse, and retrieves it without re-calculating it, if it is already 
## cached.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## To run, type, for example, "cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2)))"

cacheSolve <- function(x, ...) {
# To run, type, for example, "cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2)))"
  print("I call makeCacheMatrix the first time, the data won't be cached:")
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("not getting cached data")
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  print(m)
  print("I call makeCacheMatrix the second time, the data will be cached:")
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("not getting cached data")
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
