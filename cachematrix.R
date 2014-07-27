## Functions to cache the inverse of a matrix


## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
      
      ## Initialize the inverse
      i <- NULL
      
      ## Set the matrix
      set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
      }
      
      ## Get the matrix
      get <- function() {
            ## Return the matrix
            m
      }
      
      ## Set the inverse of the matrix
      setinverse <- function(inverse) {
            i <<- inverse
      }
      
      ## Get the inverse of the matrix
      getinverse <- function() {
            ## Return the inverse
            i
      }
      
      ## Return a list of the functions
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix" above. If the inverse has already been
## calculated then "cachesolve" should return the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      
      ## Return inverse if it is already set
      if(!is.null(m) ) {
            message("Get cached matrix")
            return(m)
      }
      
      ## Get the matrix from the object
      data <- x$get()
      
      ## Calculate the inverse
      m <- solve(data) %*% data
      
      ## Set the inverse to the object
      x$setinverse(m)
      
      ## Return the matrix
      m
}
