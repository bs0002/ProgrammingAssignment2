## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL # initialize m
  get <- function() x # get the value of x 
  setInverse <- function(inv) m <<- inv # assign value for m from another environment. Specifically, it is when cacheSolve assigns an inverse of x to m.
  getInverse <- function() m # get the value of m
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse) # output a list containing the above functions.
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  m <- x$getInverse() # x is an instance of makeCacheMatrix, getInverse will retrieve the value of m (the inverse), which is part of instance x.
  if(!is.null(m)) # m is either null or has been assigned a value of inverse of the matrix contained in instance x. 
  {               # If it has been assigned a value (by calling cacheSolve once), then m is not null, and it will skip the computation.
    message("getting cached data")
    return(m)
  }
  data <- x$get() # get the unsolved matrix.
  m <- solve(data, ...)
  x$setInverse(m) # set the inverse of the matrix of instance x to its m value
  m
}
