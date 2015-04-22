## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Great explanation: https://class.coursera.org/rprog-013/forum/thread?thread_id=125#post-700


makeCacheMatrix <- function(x = matrix()) {					##
		m <- NULL											 # create 'm' variable
		set <- function(y)
					 {
                				x <<- y
                				m <<- NULL
      	 			  }
        get <- function() x									# Returns original matrix
        setinverse <- function(solve) m <<- solve			# Modify existing matrix
        getinverse <- function() m							# Returns matrix inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}
 


# Returns cached matrix inverse using previously computed matrix inverse
cacheSolve <- function(x, ...) {							 # This function a computes, caches, and returns matrix inverse
        ## Return a matrix that is the inverse of 'x'
 	  m <- x$getinverse()									 # Returns matrix inverse of matrix 'x' and assign to variable 'm'
        if(!is.null(m)) {									 # Check for existing data in variabe 'm' ('m' is not NULL ). If this expression TRUE^
                message("getting cached data")				 # We output information message "getting cached data"
                return(m)								     # and return 'm' value
        }
        data <- x$get()									     # Get matrix
        m <- solve(data, ...)								 # Computes matrix inverse
        x$setinverse(m)										 # Modify existing matrix
        m						
}
