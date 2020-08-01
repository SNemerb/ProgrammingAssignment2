## The subsequent functions allow to store the inverse of a matrix in cache in 
## order to save computation time.

## The first function, makeCacheMatrix(), sets and gets the matrix of interest
## and sets and gets its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setInv <- function(inverse) {m <<- inverse}
    getInv <- function() {m}
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The second function checks whether the inverse of the matrix of interest is
## has already been stored and, if yes, returns it with a comment stating that 
## it is returning the stored inverse. If the inverse has not yet been 
## calculated, the function does so and returns the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
            message("Getting cached data")
            return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInv(m)
        m
}
