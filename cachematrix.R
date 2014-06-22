##R Programming
##Assignment 2
##Jeff P
##6/21/14

## Functions demonstrate lexical closures.  The matrix object created by makeCacheMatrix
## holds a matrix and its inversion after cacheSolve is applied.  If the object's inversion 
## has already been calculated, cacheSolve will retrieve its inverse from the cache.


## Creates a matrix object that can cache its inverse.  
makeCacheMatrix <- function(x = matrix()) {
  
  ## sets inverse matrix cache to NULL when matrix object is first created
  im <- NULL
  
  ## sets original matrix and sets inverse matrix cache to NULL whenever new matrix data is set
  set <- function(x) {
    x <<- x
    im <<- NULL 
  }
  
  get <- function() x 
  
  setinverse <- function(cachedinverse) im <<- cachedinverse
  getinverse <- function() im 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ##allows accessing functions with $
  
}


## Computes the inverse of a matrix created by makeCacheMatrix.  Retrieves inverse from cache if it
## has already been calculated.
cacheSolve <- function(x, ...) {
  
  im <- x$getinverse()
  
  ##checks if the inverse has already been calculated, if so, retrieves from cache
  if(!is.null(im)) {  
    message("getting cached data")
    return(im) 
  }
  
  ##gets original matrix from cachematrix object
  data <- x$get() 
  
  ##calculates inverse matrix
  im <- solve(data, ...) 
  
  ##caches inverse matrix
  x$setinverse(im) 
  
  im
}
