## Creates a special matrix that is actually a list of functions that allows you to store
## a matrix.  You can then use the second function to calculate the inverse of that matrix.
## the answer will be stored in cache.  The next time you calculate the inverse it will be
## retreived from cache rather than be recalculated

## Function creates a special "matrix" which is really a list of functions to work on the matrix 
## stored

makeCacheMatrix <- function(x = matrix()) {
            s <- NULL
            set <- function(y) {
                  x <<- y
                  s <<- NULL
            }
            get <- function() x
            setSolve <- function(solve) s <<- solve
            getSolve <- function() s
            list(set = set, get = get,
                 setSolve = setSolve,
                 getSolve = getSolve)
}


## First checks to see of the inverse has already been calculted.  If it has it retreives it from
## cache and returns the answer.  If it has not been calculated, it calculates the inverse and
## stores the result in cache before returning the answer.

cacheSolve <- function(x, ...) {
      s <- x$getSolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSolve(s)
      s
        ## Return a matrix that is the inverse of 'x'
}
