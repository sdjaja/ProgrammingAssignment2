## This set of functions compute the inverse of the matrix
## and caches it. This is because calculating the inverse
## of a matrix is an expensive operation and caching it can
## solve performance issues.

## Function name: makeCacheMatrix(x)
## Arguments: x is a matrix
## Description:
## makeCacheMatrix creates a special "vector",
## which is really a list containing a function
## 1. set the value of the matrix and initialize the inverse
## 2. get the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function name: cacheSolve(x, ...)
## Arguments: x is a matrix and the other arguments are 
## matrices whose inverse is to be cached
## Description:
## cacheSolve checks whether the inverse has been calculated,
## if it has been calculated, then it is returned, otherwise,
## it is calculated and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
