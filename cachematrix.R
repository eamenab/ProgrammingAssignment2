## I implemented two fucntions to prevent overcalculating the inverse of a matrix
## once the inverse is calculated is stored in "cache" so when you ask again
## for the inverse it doesn't calculate it, it just returns the stored value.
## To use it you have to first use makeCacheMatrix and then use the cacheSolve

## makeCacheMatrix is the function that represents
## the chache of the matrix. It has the functions
## to set the value of the inverse and to get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL ## we set the initial value of the inverse matrix
  set <- function(y) { ## here we set a original matrix if we want to change it and set the inverse to null
    x <<- y
    invM <<- NULL
  }
  get <- function() x ## returns the original matrix
  setInvM <- function(inversematrix) invM <<- inversematrix ## set the inverse matrix
  getInvM <- function() invM ## get the inverse matrix
  list(set = set, get = get,
       setInvM = setInvM,
       getInvM = getInvM) ## list of the functions of the function
}


## This function returns the cached matrix if it exist and if not
## it calculates the inverse and the stores it in 'x' (makeCacheMatrix object)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x is a matrix created with the function makeCcheMatrix
  
  
  invM <- x$getInvM() ## gets the value for the inverse from x
  if(!is.null(invM)) {  ## checks if the inverse is calculated previusly, and if it does it returns it
    message("getting cached data") ##send a message that is cached data
    return(invM) ##returns the cached data
  }
  data <- x$get() ## since there is no cached inverse we get the original matrix from x
  invM <- solve(data, ...) ## we invert the original matrix
  x$setInvM(invM) ## here we store it in the cache of makeCacheMatrix()
  invM ##here we return the inverse that we calculated
}
