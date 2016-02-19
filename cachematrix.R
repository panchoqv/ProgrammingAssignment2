## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
