## This functions are for making inverse matrix, and caching the result.
## functions do

##makeCacheMatrix : the function that makes matrix which can contain inverseMatrix
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y = matrix()) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinvmat <- function(invmat = matrix()) inv <<- invmat
   getinvmat <- function() inv
   list(set = set, get = get,
        setinv = setinvmat,
        getinv = getinvmat)
}

## If there is no cache, Calulate and save the inversematrix of inpu matrix.
cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)
   inv
}
