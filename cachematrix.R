## R Programming - Programming Assignment 2
## Caching data outside current environment

## Creates a list of functions to set and get a matrix and the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL  ## reset to null
    set <- function(y) {
        x <<- y                ## set cached variable
        i <<- NULL             ## indicates the inverse needs to be recalculated 
    }
    get <- function() x        ## get retrieves cached value of matrix
    setinverse <- function(solve) i <<- solve   ## calculates  inverse
    getinverse <- function() i   ## retrieves cached value of inverse
    ## return list of functions to get and set matrix, and get and set inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function returns inverse of a matrix
## if the inverse has already been cached then returns cache
## must pass in list created by MakeCacheMatrix

cacheSolve <- function(x, ...) {
        ## check that matricx is invertable
        if (det(x$get()) == 0) {
            message("error - matrix is not invertable")
            return()
            }
        ## check cache
        i <- x$getinverse()
        if(!is.null(i)) {  ## if something already cached just return that
            message("getting cached data")
            return(i)
        }
        ## place the cached matrix in data
        data <- x$get()
        ## calculate the inverse and place in i
        i <- solve(data, ...)
        ## store the calculated inverse in cache
        x$setinverse(i)
        ## Return the inverse matrix
        i
}
