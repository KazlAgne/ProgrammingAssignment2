## The two functions below work together to calculate an inverse of
## a given square matrix or retrieve the calculated inverse matrix
## once it was cached

## This function creates a vector cotaining a list with 4 functions
## including set the matrix, get the matrix, set the inverse of a
## matrix and get an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix created with 
## makeCacheMatrix after checking if it has been previously 
## calculated and retrieving it from memory if it was

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
 }

