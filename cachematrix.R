## These functions take advantage of the scoping rules in R to reduce computation time related to inverting matrices.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

##This function calculates the inverse of the "matrix" created using the previous function
##However, it first checks whether the inverse of the matrix has already been calculated.
## If so, it gets the mean from the cache defined in the previous function en does not do any computation.
## If not it computes the inverse of the matrix and setzs the value of the inverse via the setmat function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}



