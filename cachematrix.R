## Programming Assignment 2: Caching the Inverse of a Matrix

## makeCacheMatrix is a function which creates a special "vector", 
## which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ##1. set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##2. get the value of the matrix
  get <- function() x
  ##3. set the value of the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  ##4. get the value of the inverse matrix
  getinverse  <- function() i
  ## return the list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, y=matirx()) {
        ## Return a matrix that is the inverse of 'x' 
  i <- x$getinverse()
  ## Check if the inverse has already been calculated 
  ## and the matrix has not changed
  if(!is.null(i)&identical(x$get(),y)) { 
    message("getting cached data")
    return(i)
  }
  ## otherwise, directly solve it 
  i <- solve(y)
  ## update the x
  x$set(y)
  x$setinverse(i)
  ## return the result i
  i
  
}
