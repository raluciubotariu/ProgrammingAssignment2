## This program makes it possible to retrieve the inverse of a generated 
## matrix computed before from the cache

## makeCacheMatrix generates a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
                  m <- NULL
                  set <- function(y) {
    
                        x <<- y
                        m <<- NULL
                 }
  
                 get <- function() x
                 setinverse <- function(solve) m <<- solve
                 getinverse <- function() m
                 list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}



## cacheSolve computes the inverse of the generated matrix; if the inverse
## was calculated before, this can be retrieved from the cache

cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
    
                 message("getting cached data")
                 return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
