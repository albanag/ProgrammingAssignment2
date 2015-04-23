## The following two functions allow to cache the inverse of the matrix. 

## This function takes a matrix in input, intializes the inverse matrix to NULL and sets up the functions necessary for getting and setting the inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inv2) inv <<- inv2
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of a cacheMatrix object. When the inverse of such a maxtrix has already been evaluated, it is retrieved from the cache, otherwise this function computes the inverse anew. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv) ) {
    ## Returns the inverse from the cache
    message("Getting the cached matrix inverse.") 
    return(inv)
  }   
  else {
    ## Computes the inverse
    mtx_x <- x$get()
    message("Computing the matrix inverse.")
    inv_x <- solve(mtx_x)
    x$setinv(inv_x)
    return(inv_x)
  }       
}
