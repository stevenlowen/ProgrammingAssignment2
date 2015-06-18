## These functions store a matrix and its inverse in one object.
## The inverse is calculated only once after each time the matrix is changed;
## subsequent calls retrieve the stored inverse rather than calculating it
## anew.  This makes sense because matrix inversion is a very CPU-intensive
## process.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## 'x' is a matrix of numeric values
	## and is assumed to be invertible

	## Return a list containing a function to
	## 1. set the value of the matrix
	## 2. get the value of the matrix
	## 3. set the value of the inverse
	## 4. get the value of the inverse

	## This work was inspired by the makeVector function
	## in the homework example.

	## 'inv' is the inverse, which is initially unknown
	## and therefore set to NULL

        inv <- NULL
        
        ## 'y' is a matrix to be assigned to x
        ## Function assigns y to x
        ## and sets the inverse to be NULL (unknown yet)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## returns the stored matrix 'x' itself
        get <- function() x
        
        ## sets the inverse ('inv') to be the result 'inverse'
        ## which is supplied outside this function
        setInverse <- function(inverse) inv <<- inverse
        
        ## returns the inverse ('inv') of the stored matrix
        getInverse <- function() inv
        
        ## list of the four functions associated with a matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then this function retrieves the inverse from the
## cache.

cacheSolve <- function(x, ...) {
	## 'x' is a matric of numerical values
	## for which the inverse is sought
	## 'x' is assumed to be invertible

	## This work was inspired by the cachemean function
	## in the homework example.

	## get the cached value of the inverse
        inv <- x$getInverse()
        
        ## if it is not NULL (and therefore valid)
        if(!is.null(inv)) {
        		## inform the user
                message("getting cached data")
                ## and return the cached value
                return(inv)
        }
        ## if we're here, then the cached inverse is NULL
        ## and therefore invalid, so we need to calculate it
        
        ## get the matrix itself
        data <- x$get()
        ## use solve() to calculate its inverse
        inv <- solve(data, ...)
        ## cache the inverse
        x$setInverse(inv)
        ## return the inverse
        inv
}
