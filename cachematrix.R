## Assignment: Programming Assignment 2: Lexical Scoping
## Student: Mario Fonseca

## Creates a special "matrix", which is really a list containing a function to
## set the value of the matrix 
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Resets the value for the inverse matrix
  inv <- NULL
  ## Setter for the matrix
  set <- function(y) {
    x <<- y
    ## When the matrix changes, then we reset the inverse matrix
    inv <<- NULL
  }
  ## Getter for the matrix
  get <- function() x
  ##Setter for the inverse matrix
  setinv <- function(theinv) inv <<- theinv
  ## Getter for the inverse matrix
  getinv <- function() inv
  ## Returns this special matrix with attributes
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## Calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting the already computed inverse matrix")    
  }  
  else{
    ## Means we have to compute it
    inv <- solve(x$get(), ...)
    ## Set the inverse parameter to the enhanced matrix
    x$setinv(inv)
  }
  ## Return the inverse
  inv
}
