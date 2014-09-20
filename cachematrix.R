## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function returns a set of helper functions to:
# - Get/Set a matrix
# - Get/Set the inverse of a matrix
makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function (y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# This functions computes the inverse of a matrix. If the result has already
# been cached before, it returns the cached result without re-computation.
cacheSolve <- function (x, ...)
{
  result <- x$getinv()
  if (!is.null (result))
  {
    message ("Retrieving cached data...")
    return result
  }
  data <- x$get()
  result <- solve (data, ...)
  x$setinv (result)
  result
}
