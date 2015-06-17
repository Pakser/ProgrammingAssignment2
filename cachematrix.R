## These functions cache the inverse of a matrix. The first 
## function "makeCacheMatrix" creates a matrix object that 
## can cache its inverse. The second function "cacheSolve" 
## computes the inverse of this matrix and stores it in cache. 
## If the inverse has already been calculated (and the matrix 
## has not changed), the cached inverse matrix is retrieved 
## from the cache.


## The makeCacheMatrix function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) s <<- solve
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the 
## "matrix" returned by  the makeCacheMatrix function.
## If the inverse has already been calculated, the inverse 
## from the cache is retrieved. 

cacheSolve <- function(x, ...) {
      s <- x$getinverse()
      if(!is.null(s)) {
            message("getting cached inverse matrix")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setinverse(s)
      s
}
