
# This is not a beginners course in R
# This is a data container holding a matrix and optionally its inverse.
# There is no validation, you can add wrong values.
# The function will return a list of 4 functions that sets or gets the matrix
# or the inverse matrix. No computation, only data storage.
makeCacheMatrix <- function(m = matrix()) {
    
    myInternalMatrix <- m
    myInverseMatrix <- NULL
    set <- function(y) {
        myInternalMatrix <<- y
        myInverseMatrix <<- NULL
    }
    get <- function() {
        return(myInternalMatrix)
    }
    setInverse <- function(solvedMatrix) {
        myInverseMatrix <<- solvedMatrix
    }
    getInverse <- function() {
        return(myInverseMatrix)
    }
    
    return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}

# This function checks if the inverse exist, if not it runs the function that calulates the inverse
# and saves it.
cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    return(inverse)
}





## These 5 rows test that I get the expected result.
#testmatris <- matrix(2:10, nrow=3, byrow=FALSE) ##Making a matrix to test the function on
#testmatris[3,3] <- 12
#tm <- makeCacheMatrix(testmatris)
#cacheSolve(tm)
#tm$get()