## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that can have its inverse cached

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL #Matrix Inverse
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(solve) I <<- solve
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates or retrieves the cached  inverse of the input matrix
## If inverse already had been calculated, the function retrives the cache so as to not repeat computations 

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
