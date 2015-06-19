## The first function makeCacheMatrix creates a list of 4 functions
## which are set, get, setinverse, and get inverse.

## The created list stores the 4 functions and also stores 2 object values
## x and i.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
}


## This function takes the created list above as an argument, checks to see if
## i contains a value and if it does returns that value, if not then the get
## function is called to pull the value from x and evaluate the inverse, set
## the inverse value and return it.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
        ## Return a matrix that is the inverse of 'x'
}
