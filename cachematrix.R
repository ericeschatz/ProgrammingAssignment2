##programming assignment 2

##this function takes a matrix as an argument, then caches the 
##inverse of thee matrix.

##makeCacheMatrix c
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y ##global variables are set here
            m <<- NULL ##global variables are set here
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,##a list of functions is stored and can be called back later in cacheSolve
             setsolve = setsolve,
             getsolve = getsolve)
}


##cacheSolve caches the solution (inverse) of the matrix and return the inverse matrix as its value
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m  ##the cached inverse is returned
}
