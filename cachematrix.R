## makeCacheMatrix and cacheSolve compute the inverse of a matrix
## efficiently, i.e. they compute the inverse if it has not been
## computed before and cache the inverse, and if the inverse is 
## again requested and the matrix is the same, the cached inverse 
## is simply returned.

## The functon, makeCacheMatrix, creates a special "matrix", which 
## is really a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y$
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) {
		inverse <<- i
	  }
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function, cacheSolve, calculates the inverse of the special
## "matrix" created with the above function. However, it first checks
## to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the inverse in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
