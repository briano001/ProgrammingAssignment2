## makeCacheMatrix: This function creates a special "matrix" object that ## can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
	set <- function(y) {
	  x <<- y
	  inv <<- NULL
	 }
	# gets the matrix
	 get <- function() x
	
	
	#sets and then gets the inverse 
	 setinverse <- function(inverse) inv <<- inverse
	
	 getinverse <- function() inv
	
	#captures within a list 
	 list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#CacheSolve will do the needed calculations to to compute the inverse and cache the result
 cacheSolve <- function(x, ...) {
	#check to see if the inverse is already computed    
	    inv <- x$getinverse()
	
	    if(!is.null(inv)) {
	
	        message("getting cached data.")
	
	        return(inv)
	    }

    data <- x$get()
    
	#Compute the Inverse
	    inv <- solve(data)
	
	#Cache the result    
	    x$setinverse(inv)
	    
	# Print out the result    	
	    inv
}

