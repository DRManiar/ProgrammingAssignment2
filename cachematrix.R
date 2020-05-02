## The 1st function makeCacheMatrix creates a special "matrix" object
## which is a list containing four fcntions. One function to:
## 1. Set the value of the matrix (and assign NULL to the inverse)
## 2. Get the value of the matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of inverse of the matrix

## The 2nd function cacheSolve calculates the inverse of the special "matrix"
## created with the 1st function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse
## function.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieve the inverse
## from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
