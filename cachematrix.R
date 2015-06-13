## The functions will take a matrix object and check if its inverse has been stored in cache
## If it is in the cache, the matrix inverse is printed.
## If not, the function calculates the inverse, stores it in cache and prints it in the console.

## makeCacheMatrix takes a matrix object as argument and assigns set and get functions for the matrix 
## as well as set and get functions for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks if the matrix object has stored in cache an inverse matrix object
## If it does, it prints the inverse matrix
## If not, its calculates the inverse using solve, stores it in cache and then prints the inverse

cacheSolve <- function(x, ...) {
            m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
