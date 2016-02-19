## These functions can be used to easily manage caching of 
## big matrices that have to be inverted. The way to use them is:
##  0) Have a matrix in the variable 'x' 
##  1) Use the makeCacheMatrix and save that value: m <- makeCacheMatrix(x)
##  2) Whenever the inverted matrix is needed, use the cacheSolve function:
##     cacheSolve(m)

## This is part of the Assignment 2 of the Coursera course "R Programming"
## by "Johns Hopkins University".

## The submitter of this assignment is Francisco QV. February 2016.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  # inverse matrix

        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i

        list(   set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Returns a matrix that is the inverse of 'x'
## 'x' is a "CacheMatrix" object

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()

        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get() # getting the data
        i <- solve(data, ...) # calculating the inverse
        x$setinverse(i) #setting the inverse for future reference
        i # returning the inverted matrix
}
