## For Programming Assignment #2

## Function: MakeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    A <- NULL
    set <- function(y) {
        x <<- y
        A <<- NULL
    }
    
    get <- function() x
    
## set the value
## get the value    
    setinverse <- function(inverse) A <<- inverse
    getinverse <- function() A
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## functions cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    invOut <- x$getinverse()
    if(!is.null(invOut)) {
        message("getting cached data.")
        return(invOut)
    }
    data <- x$get()
    invOut <- solve(data)
    x$setinverse(invOut)
    invOut
}

## Running in the Console
## Output Below
## > a = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(a)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## There is no cache in the above (first) run
## Now run cacheSolve
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Now run the cacheSolve again
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## Cached data was retrieved in the above
