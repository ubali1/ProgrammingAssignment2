## This function creates a matrix object that stores a matrix itself, it's inverse and four additional functions to
## gain access to the matrix and its inverse. This is essentially a function constructor.
## The four functions are set, get_x, set_x_inverse and get_x_inverse

makeCacheMatrix <- function(x = matrix()) {     ## creates the matrix object that takes in a matrix x
        x_inverse <- NULL                ## erases any cached inverse matrix value that may exist in variable x_inverse        
        set <- function(new_x){         ## function saves a new_x we may wish to insert into the matrix object
                x <<- new_x             ## copies the new_x into variable x in parent environment
                x_inverse <<- NULL      ## erases old cached x_inverse from previous matrix in parent environment
        }        
        get_x <- function() x           ## function returns x stored in matrix object; this can be also written with curly brackets        
        set_x_inverse <- function(new_x_inverse) x_inverse <<- new_x_inverse    ## stores incoming new inverse matrix calculated elsewhere        
        get_x_inverse <- function() x_inverse   ## function returns stored x_inverse        
        list (set = set, get_x = get_x, set_x_inverse = set_x_inverse, get_x_inverse = get_x_inverse) ## returns a list of 4 elements/functions
}


## This function takes a matrix object as a parameter and uses the object's functions to get access to the matrix itself
## and its stored inverse. The function returns the inverse of the matrix if it exists, otherwise the function will 
## calculate a new inverse using the matrix object and return the new inverse matrix.

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$get_x_inverse()  ## This function is used to get x_inverse from matrix object list x
        if(!is.null(x_inverse)){        ## If x already has an inverse matrix, then this returns it
                message("getting cached data")
                return(x_inverse)
        }
        data <- x$get_x()               ## otherwise, get matrix from the object x
        x_inverse <- solve(data, ...)   ## Calculates the inverse of matrix
        x$set_x_inverse(x_inverse)      ## Stores the inverse in the object x_inverse
        x_inverse                       ## returns the inverse to the parent scope
}

## input the following test matrix and commands to test this script: 
## > x1 <- matrix(c(1,4,3,5), nrow = 2, ncol = 2)
## > source("cachematrix.R") in your working directory
## > x <- makeCacheMatrix(x1)
## > cacheSolve(x)
## > cacheSolve(x) - repeat command to see the message "getting cached data" being displayed

## I would like to acknowledge the discussions posted in a thread by Theodore Flamouropoulos which were crucial towards
## my understanding of this script and the remit of Assignment 2. 
