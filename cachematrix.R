## The makeCacheMatrix and cacheSolve functions

## makeCacheMatrix takes a matrix and returns a list of 4 functions
## set: Sets the matrix to a new matrix
## get: gets the current matrix
## setinverse: solves the inverse of the matrix
## getinverse: returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                     ## the inverse (inv) is null until solved
  set <- function(y) {
    x <<- y                                       ## change to the new matrix scoped inside this environment
    inv <<- NULL                                  ## the matrix inverse (locally scoped) is now NULL 
  }
  get <- function() x                             ## return the matrix
  setinverse <- function(solve) inv <<- solve     ##solve the matrix
  getinverse <- function() inv                    ## return the inverse
  list(set = set, get = get,                      ##return the "special matrix"
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve either solves for the inverse of the matrix or returns the cached solutions from out "special matrix"

cacheSolve <- function(x, ...) {
                                                  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                           ##get the current inverse
  if(!is.null(inv)) {                             ##if it's currently solved return the solution
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                                 ##otherwise get the data and solve it
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
