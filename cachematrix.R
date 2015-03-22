## This file contains two functions that work together to minimize expensive
## computation of matrix inverses by caching the inverse each time it is
## calculated. If another matrix inverse is attempted, the code will use the
## cached value if the new matrix is the same as the previous matrix.

## The function makeCacheMatrix creates a matrix, which is a list containing
## the following functions:
##  1) set - sets the value of the matrix
##  2) get - gets the value of the matrix
##  3) setinv - sets the value of the matrix inverse
##  4) getinv - gets the value of the matrix
##  5) getinvmat - sets the value of the matrix used to calculate the inverse
##  6) setinvmat - gets the value of the matrix used to calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    n <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    setinvmat <- function(mat) n <<- mat
    getinvmat <- function() n
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}


## The function cacheSolve calculates the inverse of a matrix (no error
## checking performed to insure the matrix is invertible). The input matrix, x,
## should be created using the makeCacheMatrix function. The function
## cacheSolve will:
##  1) Check to see if there is an existing cached matrix inverse
##  2) Check to see if the current matrix value is the same as the one used to
##     calculate the cached inverse.
## If those two conditions are satisfied, the cached matrix will be returned.
## If not, then the matrix inverse will be calculated using the solve function.
## Both the newly calculated inverse and the input matrix will be cached for
## use in subsequent calculations.

cacheSolve <- function(x, ...) {
    curMat <- x$get()
    lastMat <- x$getinvmat()
    invMat <- x$getinv()
    if ((!is.null(invMat)) && (all.equal(curMat,lastMat))) {
        message("getting cached data")
        return(invMat)
    }
    m <- solve(curMat)
    x$setinv(m)
    x$setinvmat(curMat)
    m
}
