## This source file contains 3 functions
## 1. makeCacheMatrix - creates a special matrix object to cache its inverse
## 2. cacheSolve - will return the cached matrix if exists or builds an inverse otherwise
## 3. validate   - will stop processing if the incoming matrix is not a square matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse
## input:  matrix
## output: list containing functions to
## 1. get - get the matrix
## 2. set - cache the matrix
## 3. setInverse - cache the inverse Matrix
## 4. getInverse - get the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {

    # validates if the matrix can be inversed or not
    validate(x)
    
    ## initialize inpMatrix & invMatrix member variables
    inpMatrix <- x                   ## inpMatrix holds the actual matrix
    invMatrix <- NULL                ## invMatrix holds the inverse of the matrix inpMatrix
    
    ## set function
    set <- function(input)
    {
        ## save/cache the incoming matrix
        inpMatrix <<- input
        
        ## clear the inverse matrix
        invMatrix <<- NULL
    }
    
    # returns the input matrix
    get <- function() inpMatrix
    
    # caches the inverse of the matrix
    setInverse <- function(inverse) invMatrix <<- inverse
    
    # returns the cached inverse matrix
    getInverse <- function() invMatrix

    # build a list of functions that can be invoked on the special matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve - looks up in the cache to see if the inverse exists.
## If inverse matrix exists in the cache
##      It will return the cached matrix 
## else 
##      1. It will build a new inverse matrix using solve function
##      2. Saves it to cache 
##      3. Returns the inverse
##
## input : special matrix built using makeCacheMatrix
## output: inverse of the cached matrix

cacheSolve <- function(x, ...) {

    ## get the inverse matrix saved in the cached matrix
    inverse <- x$getInverse()
    
    ## if the inverse in cached matrix is not null return inverse
    if (!is.null(inverse))
    {
        message("getting cached inverse of the input matrix")
        return(inverse)
    }
    
    ## get the actual matrix
    inMatrix <- x$get()
    
    ## perform inverse using solve function
    inverse <- solve(inMatrix)
    
    ## save the inverse to cache
    x$setInverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}


## stops the execution if the incoming function cannot be inversed
validate <- function(x)
{
    ## stop if number of rows doesnt match with number of columns
    if (nrow(x) != ncol(x))
        stop("matrix is not inversible")
}

