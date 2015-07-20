## rprog-030:  R Programming -- Programming Assignment 2

## Given in invertable matrix, cache it's inverse.  This script
## creates a custom matrix object that includes methods for creating,
## storing, and retrieving the inverse matrix
##
## Usage:
##    > m <- makeCacheMatrix( {description of/reference to square, invertable matrix} )
##    > cacheSolve(m)

library(matrixcalc) # Needed for is.singular.matrix() 

## For a provided, invertable matrix, compute the inverse and store it in the 'cache'
## property of the object.
##
makeCacheMatrix <- function(x = matrix()) {
    # If the matrix is singular, it has no inverse.  Check that before
    # attempting to cache an inverse matrix.
    #
    if (is.singular.matrix(x)) {
        message("Error:  Matrix is singular (no inverse).  Caching inverse aborted.")
        return(NA)
    }
    
    # The provided matrix is invertable, so clear the cache attribute (set to NULL) first
    cache <- NULL
    
    # Now, create the functions
    
    # $set -- Sets 'x' in the working environment to the matrix being inverted, and clears
    # the inverted matrix in cache (if it exists)
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }

    # $get -- Retrieve the matrix
    get <- function() x
    

    # $setMatrix -- Invert the matrix and store the inverted version in cache
    setMatrix <- function(inverse) cache <<- inverse
    
    # $getInverse -- Retrive the inverse matrix from cache
    getInverse <- function() cache

    # Return to the caller a list of all created methods    
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInverse = getInverse)
}


## Return the inverse of the matrix 'x', either from cache (if it exists),
## or via the inverse() function (caching the inverse matrix) if the cache
## is empty.
##
cacheSolve <- function(theMatrix, ...) {
    # First, check the cache.  If x$cache is empty, cache gets set to 'NULL'
    theInverse <- theMatrix$getInverse()
    
    # If cache (the returned inverted matrix) isn't null, return it ...
    if (!is.null(theInverse)) {
        return(theInverse)
    }
    
    # .. otherwise, create the inverse matrix by retrieving the original matrix
    # and feeding it through solve() to invert it
    m <- theMatrix$get()
    
    # Because invertability is checked during the makeCacheMatrix() call, this is
    # somewhat redundant, but double-check that the matrix is invertable before
    # trying to invert it (otherwise solve() will throw an error)
    if (is.singular.matrix(m)) {
        message("Error:  Matrix is singular (no inverse).")
        return(NA)
    }

    # Invert the matrix and save it to 'theInverse'
    theInverse <- solve(m, ...)

    # Store the inverted matrix into cache
    theMatrix$setMatrix(theInverse)

    # return the inverted matrix
    return (theInverse)
}
