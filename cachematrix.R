## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x= matrix())
{
  inv <-NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function(){x}
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse)
  {
    inv <<- inverse
  }
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cache")
    return(inv)
  }  
  ## Get the matrix from our object
  mat <- x$get()
  inv <- solve(mat, ...)
  ## Set the inverse to the object
  x$setInverse(inv)
  inv
}

