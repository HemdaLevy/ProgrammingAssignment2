## The following functions allow to invert an invertible matrix
## and maintain the result (cache it), so that it would be used for next reading
## without re-calculating the inverse, unless the data changes

## makeCacheMatrix prepares a sort of "box" to contain and preserve the data (both input and output).
## It returns a new list which contains the input matrix, the inverted matrix
## and some inner functions, for setting and getting these values 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cahcheSolve aims to return the result of the inversion, by calculating it if needed
## It access the list created by the above function and
## performs the inversion and stores it, if needed, and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}
