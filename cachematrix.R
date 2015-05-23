## I simply used the pattern that was shown in the leadup to the assignment
##  
## This first function creates the matrix object that contains the methods for accessing and creating the matrix Object and its inverse.  Passing a matrix while instantiation will set the object (you can later change this by using the set method).  Callling setInverse will set the Object inverse, and getInverse will call the inverse.


makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverseMatrix)  inverse <<- solve(inverseMatrix)
	getInverse <- function()  inverse
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This method takes the matrix object. This will call the objects getInverse to get at its cashed inverted matrix.  If its not there then it will call the matrix with the get method, solve the returned matrix, set the objects inverted mtrix, then return the inversion for viewing.

cacheSolve <- function(x, ...) {
         m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m

}
