## The functions get a matrix and its inverse. The first function does this more 
## slowly and in more steps, whereas the second function does this by first 
## checking to see if the first function already performed this; if so, the second 
## function skips the computation of the inverse. If the inverse has not been computed
## in the first, the inverse is computed in the second function.

## This function sets the value of the matrix, gets the value of the matrix, 
## sets the value of the inverse amd gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function calculates the inverse of the matrix created from the 
## above function. It checks first to see if the calculation has been 
## done. If so, it retrieves the inverse and skips computing; otherwise
## it calculates the inverse and sets the value of the inverse in the 
## cache via the setsolve function. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
