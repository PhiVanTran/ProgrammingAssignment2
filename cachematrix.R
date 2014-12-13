## My two functions are used to create a special matrix that stores a special matrix
## and cache's its inverse through the R command solve.

## Function "makeVector" creates a special "matrix", which is really a list containing a function to:
## 1:   set the value of the Matrix
## 2:   get the value of the Matrix
## 3:   set the value of the Inverse of matrix
## 4:   get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## Check if not null, Retrun value read from Cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## Returm inverse
  m
}
