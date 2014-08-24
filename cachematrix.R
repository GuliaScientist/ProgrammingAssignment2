## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setmatrix <- function(inverse) inv <<- inverse
      getmatrix <- function() inv
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x,...) {
## Return a matrix that is the inverse of 'x'
      inv <- x$getmatrix()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setmatrix(inv)
      inv
}
