## This pair of functions should compute and cache the inverse
## of a given invertible matrix. 

## To be honest, I do not understand exactly what the code here
## means.

## Mathematically speaking, here is why one might want to store
## the inverse of a given matrix in an easily retrievable form:
## suppose you need to solve the matrix equation Ax=B (where A 
## and B are matrices and x is a vector, all of compatible
## dimensions) for x. The solution, assuming A is invertible, is
## x = A^{-1}B. The sequence of row operations needed to produce
## this solution also gives you the inverse matrix A^{-1}. If
## you now need to solve many equations of the form Ax=B_i (for 
## i ranging from 1 to some large N), then rather than taking
## the time to do a bunch of row operations each time, you can 
## cache the inverse matrix A^{-1} and then just make a bunch of
## (comparatively quick) calculations A^{-1}B_i.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The next function returns a matrix that is the inverse of 
## 'x'. If that object is
## already cached, the function should retrieve it; if not, 
## the function should compute it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
