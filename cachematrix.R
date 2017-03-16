## The purpose of these functions is to return the 
## inverse of a matrix, either by calculating it 
## from data passed to the funciton, or by simply
## retrieving it from cached memory if the calculation
## has already been performed.

## makeCacheMatrix initializes a new
## matrix and creates an new object of variables and functions 
## that are later used in cacheSolve to determine
## if the inverse needs to be calculated or retrieved
## from cache memory

makeCacheMatrix <- function(X=matrix()) {
    inv <- NULL
    set <- function(Y){
        X <<- Y
        inv <<- NULL
    }
    get <- function () X
    setinv <- function(solveM) inv <<- solveM
    getinv <- function () inv
    list (set=set, get = get, setinv = setinv, getinv=getinv)
}


## cacheSolve takes as an input argument the object created 
## by makeCacheMatrix and either calculates the matrix's 
## inverse (and saves it in cache memory) or retrieves it 
## from cache memory

cacheSolve <- function(X, ...) {
    inv <- X$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data)
    X$setinv(inv)
    inv
}
