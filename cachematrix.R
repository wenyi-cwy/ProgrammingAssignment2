## makeCacheMatrix stores a list of functions (set, get, setmatrix and getmatrix)
## makeCacheMatrix takes an invertible matrix as input

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get, 
       setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve first checks whether the value m is not null, and returns the message if m is already stored as cache
## if m is null, cacheSolve instead calculates the inverse
## cacheSolve takes the object created with the function makeCacheMatrix as input

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix=(m)
  m
}
