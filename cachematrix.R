## The two functions written here creates a special matrix that can cache the
## solution of matrix inversion

## Write a short comment describing this function
## Description: makeCacheMatrix creates a special "matrix" object for caching
## the value of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(z){
      x <<- z
      s <<- NULL
  }
  get <- function() x
  setsolution <- function(solution) s <<- solution
  getsolution <- function() s
  list(set = set, 
       get = get, 
       setsolution = setsolution,
       getsolution = getsolution)
}



## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If s, the solution value, has already been calculated and cached, then s
## is returned without further calculation. Otherwise the function proceeds to
## calculate matrix inversion.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolution()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolution(s)
  s
}

