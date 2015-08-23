## Coursera R Programming Assignment 2
## Author:  Github user Patersilvae
## Date:  8/23/15
## This file contains two functions:
##  (1) makeCacheMatrix creates a new "matrix" object that takes a square, invertible matrix
##      as an input and then caches its inverse if it is calculated via the cacheSolve function
##  (2) cacheSolve checks to see if the makeCacheMatrix object already has a calculated inverse.
##      If it does, this function returns the cached inverse.  If not, this function calculates
##      the inverse, saves it in the cache, and then returns it.
## 

## makeCacheMatrix produces a new "matrix" object that contains a supplied, square invertible matrix
##  as well as caches the matrix's inverse once it is calculated.  This object also has 
##  functions for setting and getting the value of the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {  ## function takes the input square invertible matrix
                                             ## and assigns it to the variable 'x' 
        inv <- NULL   ## sets the cached inverse matrix to 'NULL' so we may check whether it exists
        set <- function(y){   ## defines the 'set' function which allows us to reset the matrix
                              ## in the object
          x<<-y     ## redefines the matrix to the provided 'y'
          inv<<-NULL  ## indicates that no inverse has been calculated for the new matrix
        }
        get <- function() x   ## defines the 'get' function for recovering the original matrix
        setinv <- function(inverseM) inv <<- inverseM  ## defines the 'setinv' function for setting the inverse matrix; this is called by the cacheSolve func
        getinv <- function() inv   ## defines the 'getinv' function which returns the cached inverse matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)  ## defines the list made by 
                                                              ## makeCacheMatrix
      }


## cacheSolve returns the inverse of the makeCacheMatrix object passed to it.  It does this
## by first checking to see if a solution has already been cached in the object.  If the solution
## is cached, it returns the cached data.  If the inverse has not been calculated and stored,
## this function solves for the inverse using R's "Solve()" function and returns the result.

cacheSolve <- function(x, ...) {  ## define the cacheSolve function that takes a makeCacheMatrix object as an input
        ## Returns a matrix that is the inverse of 'x'
        inv <- x$getinv()  ## Checks to see if the 'x' object already has a cached inverse.  If so, this value is passed to 'inv.'
        if(!is.null(inv)){  ## Checks if 'inv' has a value or is still NULL
              message("getting cached inverse matrix")  ## if 'inv' exists, we are using a cached result
              return(inv)  ## return the previously cached result; end execution 
        }
              ## If no cached inverse was found, we move on to calculate the inverse.
        matr <- x$get()  ## Pulls the matrix data from the 'x' object
        inv <- solve(matr,...)  ## Solves for the inverse of the matrix now in 'matr'
        x$setinv(inv)  ## Sets the object 'x' inverse matrix cache to the result from the line above
        inv  ## Returns the calculated inverse matrix.
}
