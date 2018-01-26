## The two functions below are used to take an invertible matrix and calculate its inverse. 
## The inverse matrix is then cached for reuse. 
## If a new matrix is introduced the new inverse will be calculated and cached for reuse.

## makeCacheMatrix takes an invertible matrix as an argument and creates a list of functions to:

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) m <<- new_inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a makeCacheMatrix object as an argument and returns the inverse of the matrix referred to in the makeCacheMatrix object. 
## It checks to see if the inverse has already been calculated. 
## If so, it returns the inverse from the cache. 
## If not, it calculates the new inverse and returns that.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
