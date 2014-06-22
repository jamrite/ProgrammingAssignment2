## makeCacheMatrix and cacheSolve are a set of functions
## that work together to cache the inverse of a matrix

## makeCacheMatrix creates a special list that can hold
## a matrix 'x' and its inverse 'inv' as variables.
## makeCacheMatrix returns a list of get and set methods 
## for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) { 			## makeCacheMatrix accepts a matrix 'x' as an argument
														## 'x' is set to an empty matrix if not specified by caller
		
		inv <- NULL										## Set value of 'inv' to NULL every time makeCacheMatrix is called
		
        set <- function(y) {							## Sets the value of 'x' to passed matrix 'y'. 
														## This should a have the same effect as calling makeCacheMatrix(y)
														
                x <<- y									## Use <<- operator to assign value of 'y' to 'x' (which is defined in the parent environment)
                inv <<- NULL							## Since 'x' has a new value, set 'inv' to NULL
        }
		
        get <- function() x								## Returns the value of 'x'
		
        setinverse <- function(inverse) inv <<- inverse	## Sets 'inv' to the passed matrix 'inverse'
														## Does not actually calculate the inverse	
														
        getinverse <- function() inv					## Returns the value of 'inv'
		
        list(set = set, get = get,						## makeCacheMatrix returns a list of the four functions defined above
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve accepts the special list, 'x' created by makeCacheMatrix
## and returns the inverse of the the matrix contained in 'x'
## cacheSolve returns a cached value of the matrix, if its available

cacheSolve <- function(x, ...) {						## Accepts output of makeCacheMatrix
        
		inv <- x$getinverse()							## Assigns output of the 'getinverse()' method of the input list to 'inv'	
        if(!is.null(inv)) {								## If 'x' contains cached inverse matrix, returns it with a message
                message("getting cached data")
                return(inv)								## Function exits here if cachec value is found
        }	
        data <- x$get()									## If cached value is not found, get the matrix in the input list 'x'
        inv <- solve(data, ...)							## Calculate inverse of matrix using the solve(...) method
        x$setinverse(inv)								## Cache value of inverse matrix in x for the next time
        inv												## Return inverse matrix 'inv'
}
