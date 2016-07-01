## Week 3 Programming Assignmnet 
## for Caching the Inverse of a Matrix

## Write a short comment describing this function
## The following function will cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(inverse) m <<- solve
      getsolve <- function() m
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The following function will compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
