## Put comments here that give an overall description of what your
## functions do :Below are two functions that are used to create a special 
## object that stores a matrix and cache's its inverse matrix.

## Write a short comment describing this function: The first function, makeCacheMatrix  creates

## a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ainv <- NULL
  set <- function (y) {
    x<<-y
    ainv<<- Null
  }
  get <- function() x
  setinv <- function(solve) ainv<<-solve
  getinv <- function () ainv
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)

}


## Write a short comment describing this function: The following function calculates the
## inverse of the special "matrix" created with the above function. However, it first checks 
## to see if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setmean function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ainv <- x$getinv()
  if (!is.null(ainv)) {
    message ("getting cached data")
    return(ainv)
  }
  data <- x$get()
  ainv <- solve(data,...)
  x$setinv(ainv)
  ainv
}
