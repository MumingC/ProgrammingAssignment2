##The following pair of functions that cache the inverse of a matrix.
 

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix: create a special "matrix" object that can cache its inverse.
## Please input an matrix
## eg. m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > makecacheMatrix (m1)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x<<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted) inv <<- inverted
  getinv <-function()inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if (!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  data<-x$get()
  inv <-solve(data,...)
  x$setinv(inv)
  inv
}
