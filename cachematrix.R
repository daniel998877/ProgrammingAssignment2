## The following function will take in an square matrix and out put its inverse.
## The first function will cache the inverse for later use.

## makeCacheMatrix function takes in a matrix and creates a list object of functions.
## The get() funtion returns the recent value of the input argument, the set() function can
## cache the input value of the argument (a matrix), the setinv() caches the matrix inverse
## and the getinv() returns the cache inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()  { x }
  setinv <- function(inv) { m <<- inv }
  getinv <- function() { m  }

  list( set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve returns either the inverse of a matrix passed in or get the cached value inverse.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'

}


