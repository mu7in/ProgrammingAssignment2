##  This function creates a special "matrix" object, which is really a list
##  containing a function to:

##  Set the value of the matrix

##  Get the value of the matrix

##  Set the value of the inverse

##  Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property to NULL
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y       ## Assign new value to matrix in parent environment
    inv <<- NULL  ## Reset the inverse if a new matrix is set
  }
  
  ## Method to get the matrix
  get <- function() {
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Method to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  ## Return a list of the methods
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix.If the inverse has already been calculated (and the matrix 
##  has not changed), then cacheSolve will retrieve the inverse from the cache 
##  and skip the computation.

cacheSolve <- function(x, ...) {
  ## Attempt to get the inverse from the cache
  inv <- x$getInverse()
  
  ## If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If not cached, get the matrix itself
  mat <- x$get()
  
  ## Calculate the inverse using the solve() function
  inv <- solve(mat, ...)
  
  ## Cache the calculated inverse for future use
  x$setInverse(inv)
  
  ## Return the inverse
  inv
}