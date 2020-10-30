## This file contains the functions for assignment 2.
## "makeCacheMatrix": Allows to create a matrix object which uses a list as a container. 
## "cacheSolve": Allows to create a matrix object which uses a list as a container. 

#  Based on makeVector and cachemean, respectively (https://github.com/rdpeng/ProgrammingAssignment2).




## makeCacheMatrix: Allows to instantiate a special matrix object. 
#                   It stores a matrix and its inverse in a list.

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function()
  {
    x
  }
  setInverse <- function(x_inv)
  { 
    m <<- x_inv
  }
   
  getInverse <- function()
  {
    m
  }
    
  #Output of the function
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## cacheSolve: Returns the inverse "x" of the special matrix object "x" created by 
#              makeCacheMatrix. 
#              It computes a conventional matrix "m".

cacheSolve <- function(x, ...) 
{
  # Return a matrix m that is the inverse of 'x' 
  m <- x$getInverse()
  if(!is.null(m)) 
  {
    message("Matrix::")
    message(str(x$get()))
    message("Getting cached inverse matrix:")
    message(str(m))
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  #Output of the function
  m
}
