
## THis function creates a matrix object that can cashe its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setI <- function(solve) I <<- solve
  getI <- function() I
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}


## This function computes the inverse of a matrix returned by FUN makeCasheMatrix above
## If the inv. has already been calc'd and the matrix has not changed, then this
## function retrieves the inverse already stored in the cashe.

cacheSolve <- function(x, ...) {
  I <- x$getI()
  if(!is.null(I)) {
    message("getting cashed data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setI(I)
  I
}
