## The following functions creates a special matrix object that can cache its 
## inverse and computes the inverse of the special matrix.

## This function creates a special object that stores a matrix and caches its
## inverse
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
   set <- function(y) {
           x <<- y
           i <<-NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse 
   getinverse <- function() i
   list(set = set , get = get ,
        setinverse = setinverse ,
        getinverse =getinverse)

}


## This function calculates the inverse of the special matrix returned by the 
## makeCacheMatrix function above
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
