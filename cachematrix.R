## Put comments here that give an overall description of what your
## functions do

## The pair of functions makeCacheMatrix() and cacheSolve()enable the caching of the
## inverse of the matrix. Caching prevents unnecessary repetition of the inverse operation.


## Written by Naveen Thalanki - 5.21.2015


## The function makeCacheMatrix()provides 4 sub functions that help us store and retrieve 
## the inverse of the matrix passed into the function as the argument

makeCacheMatrix <- function(x = matrix()) {
	inverse_m <- NULL
	
	## set is a function that changes the matrix argument stored in the main function
	set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
      ## get is a function that returns the matrix x argument stored in the main function
	get <- function(){ 
			x
	  }

      ## the setinverse function changes the matrix variable in the 'main' function to the argument value
	setinverse <- function(inv) {
				inverse_m <<- inv
			}
	## the getinverse function returns the inverse matrix set in the main function. If null it returns null
      getinverse <- function() {
				inverse_m
			}
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## The function cacheSolve() checks to see whether matrix inversion was done or not in a 
## prior operation. If not then it inverts the matrix and stores it in the object created by the
## makeCacheMatrix() function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmatrix <- x$getinverse()
	
	## if the output of the getinverse function is not null then return its value and exit this function
	if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
	
	## if we are here then no inverse matrix exists so we create it and cache it using the setinverse function
	originalmatrix <- x$get()
      invmatrix <- solve(originalmatrix, ...)
      x$setinverse(invmatrix)
      invmatrix
}
