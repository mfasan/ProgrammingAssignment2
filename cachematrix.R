## The following two functions allows the inverse of a matrix to be cached.
## Rather than having to recalcualte the inverse since this can be 
## compuationally intensive.

## makeCacheMatrix takes a matrix as its argument, builds a set of 
## functions (set, get setinv, getinv) and returns the functions 
## within a list to the parent environment. The setinv function uses
## the solve function in R to calculate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## cacheSolve tries to retrieve the inverse of the matrix passed to it 
## using getinv function. If the returned value if not null, it implies
## that the matrix has not changed and the inverse was previously 
## calculated and the cached value can be returned. Otherwise, the 
## inverse is calculated and cached.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
			message("Getting cached inverse")
			return(inv)
	}
	matr <- x$get()
	inv <- solve(matr, ...)
	x$setinv(inv)
	inv
}
