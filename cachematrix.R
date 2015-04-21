makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL	# inv_m will cache my inverse matrix
    set <- function(y) {	# setter definition
        x <<- y
        inv_m <<- NULL
    }
    get <- function() { x } # getter definition - just returns x
    
    setinverse <- function(solve) { inv_m <<- solve } # assigns the inverse
    getinverse <- function() { inv_m } # just returns de inverse matrix
    
    # list of my four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
	# trying to recover my inverse matrix
    inv_m <- x$getinverse()
    
    if(!is.null(inv_m)) {
		# if inverse is not null, we succesfully get the cached value
        message("getting cached data")
        return(inv_m)
    }
    # if not, we make the operation and set the inverse
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setinverse(inv_m)
    inv_m
}
