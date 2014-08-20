## cachematrix.R - Programming Assignment 2: Lexical Scoping
## Below are two functions that are used to create a special matrix object 
## that stores a numeric matrix and cache's its inverse.
## (Example used: Caching the Mean of a Vector)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(mtx) inv <<- mtx
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## This function calculates the inverse of the special "matrix" object 
## created with the above function. 
## 
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

## How to test:
## source ("cachematrix.R")
## c <- rbind(c(1, -1/4), c(-1/4, 1))
## class(c)
## dim (c)
## cc <- makeCacheMatrix(c)
## 
## cacheSolve(cc)
## 
## getting cached data
## cacheSolve(cc)
##