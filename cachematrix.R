## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## to create a list containing a function to
        ##   1. set the value of a matrix
        ##   2. get the value of the matrix
        ##   3. set the inverse of the matrix
        ##   4. get the inverse of the matrix
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## check the cache first
        ## if no cached inverse 
        ## calculate the inverse of 'x'
        ## and set the value the inverse in the cache
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
