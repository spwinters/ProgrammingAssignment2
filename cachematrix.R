## These two functions, makeCacheMatrix and cacheSolve, allow for caching the 
##   inverse of a matrix.  The makeCacheMatrix function provides functions to 
##   return a stored matrix, cache the inverse of the matrix, and return the 
##   cached inverted matrix.  The cacheSolve function computes the inverse of a
##   matrix stored by makeCacheMatrix, if it has not already been computed and
##   cached.  If it has been cached, cacheSolve will simply return the cached 
##   value.


## makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    ## save a given matrix to x, and initialize variable for an inverse matrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    ## return the saved matrix
    getMatrix <- function() x
    ## caches a given inverted matrix
    cacheInverseMatrix <- function(thisMatrix) inverseMatrix <<- thisMatrix
    ## returns cached inverse matrix
    getInverseMatrix <- function() inverseMatrix
    ## list relevant variables
    list(set = set, getMatrix = getMatrix,
         cacheInverseMatrix = cacheInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve computes the inverse of a "matrix" returned by makeCacheMatrix
##   If the inverse has already been calculated, and the matrix has not changed, 
##   cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## get cached inverted matrix
    inverseMatrix <- x$getInverseMatrix()
    ## return inverted matrix if the cached data isn't NULL
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    ## get saved (non-inverted) matrix
    data <- x$getMatrix()
    ## invert saved matrix
    inverseMatrix <- solve(data, ...)
    ## cache inverted matrix
    x$cacheInverseMatrix(inverseMatrix)
    ## return inverted matrix
    inverseMatrix
}
