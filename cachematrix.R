## makeCacheMatrix is a function that creates a special "matrix" object that can cache the inverse of a given matrix.
## cacheSolve is a function that computes the inverse of the matrix specified in the makeCacheMatrix function argument
## or,if the inverse value already exists, it displays it cached value.

## Creates "matrix" object with the ability to cache the value of the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set =set, get =get, setinv = setinv, getinv = getinv)

}


## Takes the output of makeCacheMatrix and calculates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
