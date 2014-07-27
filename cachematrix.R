## These functions work together to create a special object that stores
## a matrix and caches its inverse.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x',
## first checking whether the inverse has already been calculated,
## retrieve the inverse from the cache if it has.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()   
        if(!is.null(s)) {     ## test for existing inverse
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

