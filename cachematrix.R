## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - Creates a matrix with cached state. Returns a list of functions to get and set the matrix data
## and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL

    # The set function will fill the matrix with the data provided in matrix "y"
    # x_inv is set to NULL to ensure that the inverse is calculated the next time cacheSolve is called
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }

    # Returns a matrix with the data in "x"
    get <- function() {
        x
    }

    # Set and get the inverse of "x"
    setinverse <- function(inverse) {
        x_inv <<- inverse
    }

    getinverse <- function() {
        x_inv
    }

    # Return a list of these matrix functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of the cached matrix "x" and sets the inverse property in "x"

cacheSolve <- function(x, ...) {
        # Just return the inverse if it's already been calculated
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }

        # Otherwise calculate the inverse, store it in "x" for future references and return the value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
