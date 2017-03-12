## (C)march 2017, Boris Sebosik

## this function takes a matrix as an input,
## calculates and stores the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y=matrix()){
                x <<- y
                inverse <<- NULL
        }
        get <- function(x) x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(get=get, set=set, getinverse=getinverse, setinverse = setinverse)
        
}


## this function returns the cached inverse from makeCacheMatrix,
## if there is one and the matrix has not changed. Otherwise, it returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (class(x) = matrix) {
        i <- x$getinverse()
        d <- x$get()
        if(!is.null(i) & d = x) {
                message("Retrieving cached inverse")
                return(i)
        }
        
        i <- solve(d, ...)
        x$setinverse(i)
        return(i)
        }
        message("The input is not a matrix, cannot produce inverse")
        return(NaN)
}
