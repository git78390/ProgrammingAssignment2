## Put comments here that give an overall description of what your
## functions do.
## Callculating Inverse of a Matrix is time consumming when it do on big matrix several times.
## This special library "cache" the result of matrix inverse for time saving.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. It use the special operator ->> to store
## the variable in the global env
## To use the function :
## 1/ call the 'constructeur' : mat <— makeCacheMatrix()
## 2/ set the matrix : mat$set(x)   -- where x is a matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Non-singular matrix are not tested
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## 
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
