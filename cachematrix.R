## source file contains two functions to handle the mechanims
## to calculate the inverse of a matrix in a very efficient way by caching 
## the result if does not change

## object for working whit a cacheable inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                           # inverse
  set <- function(y) {                # set data and clear cache
    x <<- y
    i <<- NULL
  }
  get <- function() x                 # get data
  setinv <- function(inv) i <<- inv   # set inverse
  getinv <- function() i              # get inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
