## Pawel Piwowarski 02.02.2017
## Functions create environment to caching inversed matrix

## Function makeCacheMatrix
## define function set, get, setSolve, getSolve
## arguments: x, default matrix
## return list of definition of function set, get, setSolve, getSolve de 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Funtcion cacheSolve inverse matrix
## Arguments: x - square matrix (matrix has to be invertible)
## Nested f function checking reversibility of matrix, in negative case return NaN
## Return inverted matrix

cacheSolve <- function(x, ...) {
        
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #f function
  f <- function(mx) class(try(solve(mx),silent=T))=="matrix"
  #check matrix
  if(f(data))
  {
    m <- solve(data, ...)
    x$setSolve(m)
    return(m)
  }
  else
    return (NaN)
  
}
