##These fucntions are in charge of creating a matrix, calculate its inverse 
##and store its value by taking advantage of lexical scoping. These functions mimic
##the example given in the homework to calculate the mean of a vector.

## The first function (makeCacheMatrix) creates a special matrix object that can cache
## the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inversa <<- inverse
  getinv <- function() inversa
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function  which calculates the inverse of the matrix returned by makeCacheMatrix.
##The function should work properly even if we already have the inverse of the matrix, hence line27-30.

cacheSolve <- function(x, ...) {
  inversa <- x$getinv()
  if(!is.null(inversa)) {
    message("getting cached result")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinv(inversa)
  inversa
}
