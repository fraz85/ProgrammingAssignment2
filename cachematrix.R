## Programming Assignment 2 - R Programming
## Data Science Specialization

## In this example we introduce the <<- operator which can be used 
## to assign a value to an object in an environment that is different 
## from the current environment. Below are two functions that are used 
## to create a special object that stores a 'matrix' and 
## caches its inverse.
## The aim of this assignment is to learn about lexical scoping 
## and closures.

## makeCacheMatrix: creates a special 'matrix'
## which is really just a list of functions to faciliate:
##   set the value of the vector
##   get the value of the vector
##   set the value of the mean
##   get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Set new value to matrix x, clear the previously inverse cached
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Return the current matrix in original form
    get <- function() x
    
    # Set m to new inversed matrix
    setinverse <- function(inv) m <<- inv
    
    # Return m, the inversed matrix
    getinverse <- function() m
    
    # Return a list with the above four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will reslove the inverse of the matrix returned 
## by the function above, and cache it.
## If the matrix has already been cached, it will be 
## retrived from the cache and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get current state of m
    m <- x$getinverse()
    
    ## Check if m, the inverse, has already been cached
    if(!is.null(m)) {
        ## Return m if already cached
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise get the original matrix
    data <- x$get()
    
    ## Solve will calulate the inverse of the matrix
    m <- solve(data)
    
    ## Cache this inversed matrix in the object x
    x$setinverse(m)
    
    ## Return the inverse 
    m
}
