## These functions can be used together to find an inverse of a square matrix. The functions utilizes the caching and scoping techniques to save/cache information in the parent environment (here CacheSolve environment) for future use. This helps in avoiding repeated calculations.

## This function can be used to create an object of a type matrix. It provides 4 elements 1) get 2)set 3) getInv 4) setInv which can be used to cache inverse of a matrix. It also warns a user if they try to create an empty matrix or a non square matrix


makeCacheMatrix <- function(x = matrix())
{
  if (nrow(x) != ncol(x) | anyNA(x))
  {
    message("Please provide a square matrix with valid values")
  }
  
  invMat <- matrix()
  
  set <- function(y)
  {
    x      <<- y
    invMat <<- NULL
  }
  
  get    <- function() x
  setInv <- function(m = matrix()) invMat <<- m
  getInv <- function() invMat
  
  list(set= set, get = get, setInv = setInv, getInv = getInv)
}

## This Calculates the inv of a matrix if it has not been calculated already and cached. 

cacheSolve <- function(x, ...)
{
  i <- x$getInv()
  
  if(!anyNA(i) & nrow(i) == ncol(i))
  {
    message("getting cached data")
    return (i)
  }
  
  data <- x$get()
  
  if (nrow(data) != ncol(data) | anyNA(data))
  {
    message("Please provide a square matrix with valid values for inverse")
  }
  else
  {
    i    <- solve(data)
    
    x$setInv(i)
    
    i 
  }
}
