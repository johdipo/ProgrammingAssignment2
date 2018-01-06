
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # Tnitializing the inversed matrix
  set <- function(y) {    # Set the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x     # Get the matrix
  setinverse <- function(solve) i <<- solve # Set the inverse of the matrix
  getinverse <- function() i    # Get the inverse of the matrix
  list(set = set, get = get,  # List the function applicable to the special matrix object
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()      # Get the inverse of the special matrix object
  
  # Checking that the inverse is not cached yet
  
  if(!is.null(i)) {     
    message("getting cached data")
    return(i)
  }
  
  # If not inversed matrix is cached, 
  
  data <- x$get()       # Get the matrix from the special matrix object defined by makeCacheMatrix
  i <- solve(data, ...) # Compute the inverse of the matrix got from the previous line
  x$setinverse(i)         # Set the inverse matrix to the object
  i
  
}
