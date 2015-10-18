## Put comments here that give an overall description of what your
## functions do
## These functions create a object that stores a numeric matrix and cache's its inverse.
## Write a short comment describing this function
## This function creates a special matrix
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL ## the initial value of minv (inversed matrix) will be null
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x ## return the  matrix
  setinv <- function(inv) minv <<- inv ## set the inversed matrix
  getinv <- function() minv ## return the inversed matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix created
## and checks to see if the inverse has already been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv() ## get the inversed matrix of the original matrix x
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  } ## checks to see if the inversed matrix has already been calculated
  data <- x$get() ## define data such as those contained in matrix x
  minv <- solve(data, ...) ##computes the inverse matrix
  x$setinv(minv) ## set to the object
  minv ## return the result
}
