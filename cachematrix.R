## R Programming - Week 3 Programming Assignment
## Create an object to store matrix data and inverse.
## The other function with calculate the inverse and store it in the above object.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## Create a Matrix Object with setter and getter methods

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a list of functions that
  ## can cache the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Assumption:
## he matrix supplied is always invertible.
## Utilize makeCacheMatrix Object
##      - Check to see if the inverse exists in cache.
##      - If it does not exist, solve()
##      - Return m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
