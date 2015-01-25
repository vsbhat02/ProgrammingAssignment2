## These functions cache values, and assuming they already exist and arguments are the same, 
## returns those cached values instead of recomputing each time 
## 
## VB: I checked these functions with the following set of fake matrices; the caching
##     happened as expected, and the reset happened as expected as well
## 
## > check.matrix <- matrix(4:7, nrow = 2, ncol = 2)
## > check.matrix.list <- makeCacheMatrix(check.matrix)

## > cacheSolve(check.matrix.list, check.matrix) 
    # 1st call, this will compute the inverse

## > cacheSolve(check.matrix.list, check.matrix) 
    # 2nd call, this will get the result from the cache

## > cacheSolve(check.matrix.list) 
    # 3rd call, this will get the result from the cache; 
    # by default, missing current.matrix argument will be replaced with stored value

## > check.matrix <- matrix(1:4, nrow = 2, ncol = 2) 
    # new matrix value

## > cacheSolve(check.matrix.list, check.matrix) 
    # 4th call, this will compute the new inverse

## >  cacheSolve(check.matrix.list, check.matrix) 
    # 5th call, this will get the result from the new cache


## VB: There isn't a whole lot of new stuff going on in this function ... it's very similar to 
##     the example makeVector() function provided as an example. 
## With makeCacheMatrix(), we can create a list of objects that contain some info about a matrix, 
## including things like:
##   - what the matrix is
##   - what the inverse of the matrix is, while also 
## (re)setting the values for: 
##   - what the matrix is
##   - what the inverse of the matrix is

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

    ## the set() function (re)initializes values for the matrix and inverse
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
    ## the get() function simply returns the matrix
  get <- function() x

    ## the setInv() function assigns the computed inverse to the stored inv value, 
	## while getInv() returns that stored inverse value
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  
  list(set = set, 
       get = get, 
	   setInv = setInv, 
	   getInv = getInv)
}


## VB: I modified the cacheSolve() function quite a bit in comparison to the sample 
##     cachemean() function provided for calculating the mean. That's because I took the 
## instruction of "If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache" quite literally.
##     So a second argument with the current version of the matrix is added, and if 
## provided (else assume it's all the same), it first checks for whether the two are 
## identical. If they are, it's safe to proceed as normal, but if not, I need to reset
## the matrix in the list object provided. 

cacheSolve <- function(x, current.matrix, ...) {

    ## first get the stored version of the matrix
  stored.matrix <- x$get()

    ## if the current.matrix argument isn't provided, we're going to assume that nothing has changed
  if (missing(current.matrix)) current.matrix <- stored.matrix

    ## then compare if the stored = current (of course it will if not current is provided)
  if (identical(stored.matrix, current.matrix)){
      ## this branch is similar to the example -- look up the stored Inverse value, if it exists
      ## then return that; else compute it, store it, and return the computed Inverse
    inv <- x$getInv()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    } else {
      inv <- solve(current.matrix, ...)
      x$setInv(inv)
      return(inv)
    }
  } else {
    ## this branch is the new one -- essentially I need to reset the matrix in the passed in 
    ## list-object using the set() function
    
    x$set(current.matrix)

      ## after that, I can solve for the Inverse and reset the Inverse too    
    inv <- solve(current.matrix, ...)
    x$setInv(inv)

    return(inv)
  }
}
