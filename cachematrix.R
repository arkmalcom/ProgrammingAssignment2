## The funcion makeCacheMatrix creates a list that allows us to "set" a special type 
## of matrix that we will be able to retrieve cached values from(the inverse, in this case)

## This function contains a list of functions that will set or get a matrix
## or set or get its inverse depending on the command called

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <-function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This will attempt to get the cached inverse of x
## then check to see if i is not null, and if it's not
## return the cached value
## else, it will calculate the inverse and assign it to 'i' 
## before returning it

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
          message("Getting cached inverse data")
          return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
