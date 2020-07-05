## Put comments here that give an overall description of what your
## functions do
## These functions written in partial fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment; week beginning January 18, 2016; GitHub user: PamlaM

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse
    
    makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
        vari <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
        set <- function(y) {                    ## define the set function to assign new 
            x <<- y                             ## value of matrix in parent environment
            vari <<- NULL                        ## if there is a new matrix, reset inv to NULL
        }
        get <- function() x                     ## define the get fucntion - returns value of the matrix argument
        
        setinverse <- function(inverse) vari <<- inverse  ## assigns value of inv in parent environment
        getinverse <- function() vari                  ## gets the value of inv where called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
        ## to the functions with the $ operator
    }
    
    
    ## Write a short comment describing this function
    ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    ## If the inverse has already been calculated (and the matrix has not changed),
    ## then cacheSolve will retrieve the inverse from the cache
    
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        vari <- x$getinverse()
        if(!is.null(vari)) {
            message("getting cached data")
            return(vari)
        }
        data <- x$get()
        vari <- solve(data, ...)
        x$setinverse(vari)
        vari
    }