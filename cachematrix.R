## Two functions to allow calculation of the inverse
##of a matrix (assuming the matrix is invertible), but
##also cache the matrix inverse and access the cached 
## value later, as a time-saving measure.

## First, a function calculate the inverse and cache the 
## value for a matrix, making a special "vector"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Second, a function to calculate the inverse of 
## the matrix created above, but only if the 
## inverse hasn't already been calculated. 
## If it has, just return the cached result. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # if m isn't null (i.e. if the matrix has been 
  # calculated and cached already), just use that
  # and exit the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise, calculate the inverse with "solve"
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
