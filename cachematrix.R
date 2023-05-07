## function creates a special matrix that can cache its inverse 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Now the next function will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        # otherwise, compute the inverse and cache it
        data <- x$get()
        inv <- solve(data, ...) #solve is special function in R to compute inverse. 
        x$setInverse(inv)
        inv
}