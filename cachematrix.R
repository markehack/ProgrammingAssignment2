
# Create a cached matrix function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {				# Initialize
        x <<- y
        inv <<- NULL
    }
    get <- function() x					# Get the matrix
    setinverse <- function(inverse) inv <<- inverse	# Set the value
    getinverse <- function() inv			# Get the inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.
# If the inverse has been calculated, the cached value is returned. 
# If the inverse has not been calculated, the calculation is performed
# set in the cache and returned 

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()			# See if we have a value
    if(!is.null(inv)) {				# If we do then
        message("getting cached data.")		#   indicate cached answer being returned
        return(inv)
    }
    matrix <- x$get()				# Recover the matrix
    inv <- solve(matrix, ...)			# Solve it
    x$setinverse(inv)				#  and cache the result
    inv
}


