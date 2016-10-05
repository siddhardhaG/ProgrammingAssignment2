## Put comments here that give an overall description of what your
## functions do

## Vector containing Set,Get, SetInverse, GetInverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { x <<- y  
                       i <<- NULL
                      }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check the cache and return inverse if available, if not available compute inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


