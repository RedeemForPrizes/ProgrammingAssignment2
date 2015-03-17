##
## Coursera Course, R Programing, Week 3
## Programming Assignment 2
## https://class.coursera.org/rprog-012/human_grading/view/courses/973493/assessments/3/submissions
##
## Written by Harvey Siewert, 15-03-14
##
## Assignment template provided by Roger D Peng
## Thanks to Hussain Boxwala for an excellent overview of Roger's example code.
## See: https://class.coursera.org/rprog-012/forum/thread?thread_id=229
##


## This function takes a matrix as an argument and caches it in a list stored in the parent environment
##
## Creates a number of functions to
## - store and retrieve the incoming matrix to/from "x"
## - store and retrieve an inverse of matrix "x" as "cachedMatrixInverse"
##
## Both "x" and "cachedMatrixInverse" are stored in the parent environment 
## by using the <<- assignment operator
## Reference: http://www.inside-r.org/r-doc/base/assignOps

makeCacheMatrix <- function(x = matrix()) {
    
        # Usage:  <variable name> <- makeCacheMatrix(<vector>)
    
        # Create local variable for the cached matrix inverse and assign NULL
        cachedMatrixInverse <- NULL

        # This creates the function used to assign the incoming matrix
        # to the variable "x"
        # "x" is stored in the parent environment by the <<- operator
        # Since "x" is a new value, the cached value for the inverse is wiped out
        set <- function(incomingMatrix) {
            x <<- incomingMatrix
            cachedMatrixInverse <<- NULL
        }
        
        # Function returns the value of "x"
        get <- function() x

        # Function to cache the matrix inverse
        # Takes a matrix as an arg and assigns it to the cached matrix inverse variable    
        setInverse <- function(solvedInverse) cachedMatrixInverse <<- solvedInverse
        
        # Returns the value of the cached matrix inverse
        getInverse <- function() cachedMatrixInverse

        # Returns a list containing the functions to set and retrieve 
        # the original matrix and a cached version of the matrix inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Takes a variable created as output from makeCacheMatrix()
## Checks if a cached value for the matrix inverse exists
## If yes, it returns the value of the cached matrix inverse
## If no, it calculates the inverse for the incoming matrix
## and stores it in the cached matrix inverse variable
##
## Presumes that the matrix received is invertible 
## For info on invertible matrixes,
## See: http://en.wikipedia.org/wiki/Invertible_matrix

cacheSolve <- function(x, ...) {

        # Usage: cacheSolve( <var created from makeCacheMatrix> )
    
        # Retrieve the value of the cached matrix inverse 
        cachedMatrixInverse <- x$getInverse()
        
        # If the cached matrix inverse is not null, 
        # return the value of the cached matrix inverse
        if(!is.null(cachedMatrixInverse)) {
            message("getting cached data")
            return(cachedMatrixInverse)
        }
        
        # Since no value was found for the cached matrix inverse
        # Retrieve the value of the original matrix from the incoming list
        # using the get() function
        myMatrix <- x$get()
        
        # Calculate the matrix inverse and store in a local variable
        myMatrixInverse <- solve(myMatrix)
        
        # Assign the value to the cached matrix inverse variable
        # using the setInverse() function
        x$setInverse(myMatrixInverse)
        
        # Return the matrix inverse
        myMatrixInverse
}
