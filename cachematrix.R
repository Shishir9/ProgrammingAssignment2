## Matrix inversion is a costly computation, hence I have created a set of two functions
## that can store the special matrux in the cache and the other than can compute the inverse
## if the inverse already does not exist in the cache.

## this function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinv(m)
  m
}

## Invoking command for the functions;

# ma<-matrix(data = c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
# a<-makeCacheMatrix(ma)
# a$get() 
# cacheSolve(a)
