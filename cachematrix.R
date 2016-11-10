
## Coursera - R Programming Language - Week 3 assignment ##
## Nov 10, 2016 ##
## Sanjay Jain, Github user: sanjayds ##
## This function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed then cacheSolve function will get the inverse from the cache instead of computing again
## ...otherwise cacheSolve will compute again and cache it.

## Write a short comment describing this function
## This function creates a matrix based on dimensions in arguments, gets the inverse of that matrix from cached values.##
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 												## Initializing the object to carry inverse matrix with NULL
	set <- function(y) {											## Creating Set function to assign new values in matrix and ...
                x <<- y
		i <<- NULL											## ...initialize object to carry inverse matrix with NULL 
	}
	get <- function() x											## Getting the values of original matrix
	setinversematrix <- function(inversematrix) i <<- inversematrix						## Cashing value of inversed matrix in object i
	getinversematrix <- function() i									## Getting the value of inversed matrix
	list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix) 	## Create list with these names so that they can be reffered with $ operator
}


## Write a short comment describing this function
## This function returns the inverse of matrix which is received as argument in function ##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinversematrix()					## Moving the existing inversed matrix value to variable i
	if(!is.null(i)) {						## If i is not null that means cached value exists so return that cached value...
		message("Getting cached data for inversed matrix")	## ...instead of calculating again the new inversed matrix
		return(i)
	}
	data <- x$get()							## If no cached inverse matrix value,get original matrix value to calulate inverse again...
	i <- solve(data, ...)						## Call solve function to calculate inverse matrix
	x$setinversematrix(i)						## Call function to cache the value of this inverse matrix
	i								## Return inverse matrix
}
