##
## makeCacheMatrix creates a "matrix" object that can cache its inverse.
##
## cacheSolve computes the inverse of "matrix", which is returned by the makeCacheMatrix function.
## If this inverse has already been calculated, it will be returned from the cache.


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix - Create a "matrix" object that can cache its inverse
## x is a square invertible matrix
  
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix <<- solve
        getinverse <- function() matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # return a list of four functions
}


cacheSolve <- function(x, ...) {
## cacheSolve - Compute the inverse of "matrix" returned by the makeCacheMatrix function
## Returns a matrix that is the inverse of 'x'
		
        matrix <- x$getinverse()
        if(!is.null(matrix)) {  # check to see if the matrix's inverse has already been calculated
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()
        matrix <- solve(data)
        x$setinverse(matrix)
        matrix
}

