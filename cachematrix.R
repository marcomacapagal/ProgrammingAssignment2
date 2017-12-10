## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## In this Programming Assignment, two functions were created to cache the inverse of matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##  The cacheSolve function computes the inverse of the special "matrix" produced by 
##  the makeCacheMatrix function. If the inverse has already been calculated 
##  and the matrix has not changed, then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        mtrx1 <- x$get()
        inv <- solve(mtrx1, ...)
        x$setInverse(inv)
        inv
}


##  Testing the makeCacheMatrix and cacheSolve functions

## > mtrx <- makeCacheMatrix(cbind(c(1.2, 3.4), c(4.3, 2.1)))

## > mtrx$get()
##      [,1] [,2]
## [1,]  1.2  4.3
## [2,]  3.4  2.1

## > mtrx$getInverse()
## NULL

## > cacheSolve(mtrx)
##            [,1]        [,2]
## [1,] -0.1735537  0.35537190
## [2,]  0.2809917 -0.09917355

## > cacheSolve(mtrx)
## getting cached data...
##            [,1]        [,2]
## [1,] -0.1735537  0.35537190
## [2,]  0.2809917 -0.09917355

## > mtrx$getInverse()
##            [,1]        [,2]
## [1,] -0.1735537  0.35537190
## [2,]  0.2809917 -0.09917355