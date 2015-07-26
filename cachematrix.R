## Put comments here that give an overall description of what your
## functions do

## The function `makeCacheMatrix` creates a function vector which is a list containing a function to
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(N) {
    M <<- N
    I <<- NULL
  }
  get <- function() M
  setinvert <- function(invert) I <<- invert
  getinvert <- function() I
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## The following function calculates the inverse of the matrix created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `solve` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinvert()
  if(!is.null(I)) {
    message("getting cached invert matrix")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinvert(I)
  I
}
