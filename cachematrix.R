## This file has a constructor function that creates a kind of wrapper
## (a class object, if you will) on the matrix passed to it.
## This wrapper class has methods to get/set the matrix's value in the wrapper,
## as well as it's inverse.
## The second function tests for the existence of the inverse by taking the 
## wrapper class object and using it's get method for the inverse. If found, it
## returns this pre-calculated inverse, else calculates anew and uses the
## wrapper class' set methods to store the original and the inverse matrix.

## makeCacheMatrix - inputs: a numeric square matrix
## test if input matrix is indeed a square matrix
## if yes, check if each row contains only numeric/integer values
## if not, throw error and stop
## if all checks are okay, create 4 functions to get/set the input matrix and
##  it's inverse
## all of these nested functions use env referencing to assign values to their
##  parent env (the makeCacheMatrix env)
## finally, create a list of all the 4 functions as the last executed statement,
##  as the return value of the function
makeCacheMatrix <- function(x = matrix()) {
    x.dims <- dim(x)
    if (length(x.dims)==2 & x.dims[1]==x.dims[2]) {
        for (i in 1:x.dims[1]) {
            if (!(class(x[i])=="numeric" | class(x[i])=="integer")) stop("Please provide a numeric matrix only")
        }
    } else stop("Please enter a square matrix only")

    x.inv <- NULL
    
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(invX) x.inv <<- invX
    
    getInv <- function() x.inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve - inputs: the return object from makeCacheMatrix function
## call getInv of the input object (a list returned from makeCacheMatrix) to
##  retrieve the inverse stored
## if returned object is not null, return that cached object and exit
## else, fetch the matrix from the input object using the get() function
## then calculate inverse using solve() in the base package
## cache the calculated inverse in the input object using the setInv() method
## return the newly calculated inverse matrix
cacheSolve <- function(x, ...) {
    inv_x <- x$getInv()
    
    if(!is.null(inv_x)) {
        message("getting inverse matrix from cache")
        return(inv_x)
    }
    
    message("inverse does not exist...calculating")
    new_x <- x$get()
    inv_x <- solve(new_x, ...)
    x$setInv(inv_x)
    inv_x
}
