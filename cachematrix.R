
## Below are two functions that create a special matrix object that cache's its inverse

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL      
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Computes the inverse of the special "matrix" with the above function

cacheSolve <- function(x = matrix(),...) {
 
  ## Return a matrix that is the inverse of 'x'
  
  ## First checks to see if the inverse has already been calculated
  ## If so it gets the inverse from the cache and skips the computation
  
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function
  
  matrix <- x$get()
  m <- solve(matrix,...)
 
  x$setmatrix(m)
  m
 
}
