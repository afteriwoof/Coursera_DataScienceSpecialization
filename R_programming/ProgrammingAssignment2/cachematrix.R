## Function to determine the inverse of a matrix and cache the result in case it is called again, using lexical scoping to save on computational time and resources (i.e. not repeat a calculation which is redundant).

## makeCacheMatrix sets the variables for the inverse matrix and the functions for either setting or getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	set <- function(y){
		x <<- y
		x_inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) x_inv <<- solve
	getinv <- function() x_inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve calls the above makeCacheMatrix functions to either return the matrix or its inverse, from either calculating it or returning the cached result if previously calculated already.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	x_inv <- x$getinv()
	if(!is.null(x_inv)){
		message("getting cached data")
		return(x_inv)
	}
	data <- x$get()
	x_inv <- solve(data, ...)
	x$setinv(x_inv)
	x_inv
}
