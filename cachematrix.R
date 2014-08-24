## These functions cache and return the inverse of a matrix.

## This function, makeCacheMatrix creates a special "vector", which is a list 
## containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # sets the value of inv to NULL
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
        inv <- x$getmatrix()    # if an inverse has already been calculated this gets it
        
        if(!is.null(inv)) {     # check to see if cache is null.
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         # Get the original matrix
        inv <- solve(data)      # Call the solve function to get the inverse of the original matrix
        x$setmatrix(inv)        # cache the inverse of the matrix.
        inv
}
