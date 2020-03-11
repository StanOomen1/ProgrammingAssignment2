#Made by Stan Oomen
#This functipm Caches the inverse of a matrix.
#It gets/sets the value of the matrix and gets/sets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}

#This function calculates the inverse of the matrix. 
#Or it gets it from cache when it is already calculated. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
