## These functions cache and return the inverse of a matrix.

## This function, makeCacheMatrix creates a special "vector", which is a list 
## containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix


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

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

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
