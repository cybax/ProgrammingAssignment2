## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakecacheMatrix function will be used to hold the cache of the Matrix and its Inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Getter and Setter functions for Matrix
        inv = NULL
        get = function() x
        set = function(y) {
                x<<-y
                inv <<- NULL
        }
        
        ## Getter and Setter functions for the inverse of the Matrix
        getInverse = function() inv
        setInverse = function(inverse)
                inv<<-inverse
        list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInverse()
       
        a <- x$get()
        inv = solve(a,...)
        x$setInverse(inv)
        inv
}
