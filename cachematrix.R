## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(1:4,2)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## makeCacheMatrix create a list with four functions
## set-> inputs in cache the matrix included in the function 
##    if nothing added default matrix is matrix (1:4,2)
## get -> can be used to get matrix stored by set in step above
## setinverse -> when cacheSolve is used, if the matrix is not saved in set
##    it stores the inverse of the matrix (calculated as solve(matrix))
## getinverse -> can be used to get the the matrix in setinverse


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
## Return a matrix that is the inverse of 'x'
## if already run cacheSolve('x') (i.e.: inverse of x already calculated)
##    the inverse matrix of 'x' saved in x$setinverse 
##    (see makeCacheMatrix) is returned
## if cacheSolve ('x') has not being calculated
##    inverse matrix is calculated and saved in x$setinverse 

##  to check that inverse is correctly calculated run
##    a<-makeCacheMatrix("Your Matrix")
##    a$get()%*%a$getinverse()
##  if it is correct identity matrix should be returned
