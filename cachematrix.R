## These two functions allow a user to cache matrix inversion values.  Use the 
## first to generate a cache.  Use the second to generate the inverse. 

## makeCacheMatrix creates a list of functions to set, and get values for a 
## matrix, and set and get inverse values for said matrix in order to 
## cache the known inverse values for matrices 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                 #initialize inverse
    set <- function(y){                         #function to set a new matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                         #return the existing matrix
    setInv <- function(inverse) inv <<- inverse #set matrix inverse
    getInv <- function() inv                    #return matrix inverse
    list(set = set, get = get,                  #return a list of functions 
         setInv = setInv, getInv = getInv)

}


## This function checks for a cached inverse matrix and generates one if not

cacheSolve <- function(x, ...) {
        inv <- x$getInv()               ##load existing mean (if it exists)
        if(!is.null(inv))               ##if it exists exit and print message
        {
            message("loading cached inverse matrix")
            return(inv)
        }
        data <- x$get()                 ##load matrix
        inv <- solve(data,...)          ##generate inverse matrix
        x$setInv(inv)                   ##set the inverse value 
        inv
}
