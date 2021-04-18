## A pair of functions capable of caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function (x=matrix()) {
  i <- NULL                                         ## initialize the inverse property as NULL (this will hold value of matrix inverse)
  set <- function(y) {                              ## define set function to assign new value to matrix in a parent environment
    x <<- y
    i <<- NULL                                    ## reset i to NULL
  }
  get <- function () x                              ## define get function to get the matrix 
  setInverse <- function (inverse) i <<- inverse    ## assigns a value to i in parent environment
  getInverse <- function () i                       ## gets the value of i 
  list (set = set, get = get,                       ## return to a list of the methods
        setInverse = setInverse,
        getInverse = getInvserse)
}
cachesolve <- function (x, ...) {                     ## cachesolve is purposed to retrieve the inverse from the cache
  i <- x$getInverse()                               ## get inverse (x) and assign to i
  if(!is.null(i)) {
    message ("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- inverse (mat, ...)
  x$setInverse(i)
  i
}