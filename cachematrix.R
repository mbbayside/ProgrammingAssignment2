## This set of functions supports calculating and caching the inverse of a 
## matrix. This is useful for pre-calculating the matrix inverse (a potentially
## costly computation) when this inverse is to be used repetitively.
##
##  -- Function makeCacheMatrix: 
##        Create an object that manages a specified matrix and its 
##        inverse calculated using the cacheSolve function.
##
##  -- Function cacheSolve:   
##         Calculate the inverse of the matrix stored in object created with
##         makeCacheMatrix. If the inverse does not already exist, it is
##         calculated and stored back in the object. Otherwise, the value of the
##         cached inverse is returned
##    
##
## -----------------------------------------------------------------------------
## Function makeCacheMatrix
##        Create an object that manages a specified matrix and its 
##        inverse calculated using the cacheSolve function.
## Inputs:
##     An invertible matrix
## Outputs:
##     A object that consists of a list of functions for managing the matrix
##     and its inverse
##           set: store the matrix and clear the inverse placeholder
##           get: retrieve the matrix
##       setminv: store the matrix inverse
##       getminv: retrieve the inverse
##
makeCacheMatrix <- function(mat = matrix()) {
        # placeholder for the cached matrix inverse
        minv <- NULL
        ## function for storing the matrix to invert and reset the inverse 
        ## placeholder
        set <- function(m = matrix()) {
                ## assign the argument m to the variable mat. This variable
                ## exists in the parent environment to this function
                mat <<- m
                ## clear the inverse, so that inverse is recalculated upon
                ## calling the cacheSolve function
                minv <<- NULL
        }
        ## function for retrieving the matrix
        get <- function() mat
        ## function for storing the inverse
        setminv <- function(invtostore = matrix() ) {
                minv <<- invtostore
        }
        ## function for retrieving the cached matrix inverse
        getminv <- function() minv 
        # output list of functions for creating/storing/retrieving the matrix 
        # and its inverse
        list( set = set, get = get, setminv = setminv, getminv = getminv)
}

## -----------------------------------------------------------------------------
## Function cacheSolve:   
##     Calculate the inverse of the matrix stored in object created with 
##     makeCacheMatrix. If the inverse does not already exist, it is calculated
##     and stored back in the object. Otherwise, the value of the cached inverse
##     is returned
## Inputs:
##    An object created by the MakeCacheMatrix function
## Outputs:
##    The inverse of the matrix in the input object (calculated or cached value)
##
cacheSolve <- function(mat, ...) {
        ## Retrieve the value stored in the inverse placeholder of mat using the
        ## getminv function
        minv <- mat$getminv()
        ## Check if there is a value alredy available
        if (!is.null(minv)) {
                ## there is a non-null value. Post a message and return the
                ## cached inverse
                message("getting cached inverse")
                return(minv)
        }
        ## The inverse placeholder is empty. Retrieve the matrix value
        data <- mat$get()
        ## Calculate the matrix inverse using the solve() function
        minv <- solve(data)
        ## store the calculated inverse in object mat using setminv
        mat$setminv(minv)
        ## return the value of the inverse
        minv
}
