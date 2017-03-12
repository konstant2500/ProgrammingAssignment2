## OVERALL DESCRIPTION OF WHAT THESE 2 FUNCTIONS DO:
## (1) The makeCacheMatrix and cacheSolve functions work as a pair 
##       to create a special "matrix" object that can cache its inverse, 
##       so that time consuming computation of the inverse does not have to be repeated 
##       if the contents of the matrix remains unchanged.


## --------------------------------------------------------------------------

## makecCacheMatrix FUNCTION DESCRIPTION:
## The makeCacheMatrix function creates a special "square matrix", 
##    which is a list containing the functions to:
##    (1) set the matrix
##    (2) get the matrix
##    (3) set the inverse
##    (4) get the inverse
## The 'list' (last statement), contains the 4 set & get functions above, 
##    and serves as input ('x') to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
   
   # Initialize cacheInverse as NULL, in the current environment
   cacheInverse <- NULL
   
   # Define the function 'set'
   set <- function(y) {
      # use the <<- operator, to assign a value to an object 
      # in an environment different from the current environment
      # (This is part of the R Scoping Rules).
      x <<- y
      cacheInverse <<- NULL
   }
   
   # Define the function 'get'
   get <- function() x
   
   # Define the function setcacheInverse
   setcacheInverse <- function(inverse) cacheInverse <<- inverse
   
   # Define the function getcacheInverse
   getcacheInverse <- function() cacheInverse
   
   # Return the 'list' of the 4 functions (via 'x')
   list(set = set, get = get, setcacheInverse = setcacheInverse, getcacheInverse = getcacheInverse)
   
}

## --------------------------------------------------------------------------

## cacheSolve FUNCTION DESCRIPTION:
## The cacheSolve function computes the inverse of the special "matrix" 
##    (created and returned by the makeCacheMatrix function above). 
## If the inverse has already been calculated (the matrix has not changed), 
##    then cacheSolve retrieves the inverse from cache, as shown below:
##    (1) The cacheSolve function below first checks 
#           to see if the inverse has already been calculated. 
##    (2) If inverse has been calculated, cacheSolve retrieves inverse from cache, 
##          returns inverse, and exits, thus skipping the computation. 
##    (3) If inverse not calculated, cacheSolve calculates inverse of data, 
##          then sets value of inverse in cache via setcacheInverse function.


cacheSolve <- function(x, ...) {
   
   # Assign the x$getcacheInverse from the makeCacheMatrix function to cacheInverse 
   # (may contain the inverse or may contain NULL)
   cacheInverse <- x$getcacheInverse()
	
	# Test if cacheInverse is NOT NULL..
   if(!is.null(cacheInverse)) {
      # If NULL test is TRUE, then matrix inverse already calculated, 
      # thus get inverse from cache, and display message.
      message("getting cached data")
      # Return the inverse of the matrix, and exit this function
      return(cacheInverse)
   }
   
   # If null test is FALSE, then create a matrix
   data <- x$get()
   # Calculate the inverse of the matrix (using 'solve')
   cacheInverse <- solve(data, ...)
   # x$setcacheInverse stores the inverse value into cacheInverse 
   x$setcacheInverse(cacheInverse)
   
   # Return cacheInverse, which is the inverse of the matrix 'x'
   return(cacheInverse)
				
}

# --------------------------------------------------------------------------



