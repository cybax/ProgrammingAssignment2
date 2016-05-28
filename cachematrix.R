## Matrix inversion is CPU intensive computation and there may be some benefit
## to caching the inverse of a matrix. The following two functions are used to cache the inverse of a matrix.

## MakecacheMatrix function will be used to hold the cache of the Matrix and its Inverse. makeCacheMatrix will include 
## 1. get the value of the Matrix
## 2. set the value of the Matrix
## 3. get the value of the inverse of the Matrix
## 4. set the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Getter and Setter functions for Matrix
        inverse = NULL
        get = function() x
        set = function(y) {
                x<<-y
                inverse <<- NULL
        }
        
        ## Getter and Setter functions for the inverse of the Matrix
        getInverse = function() inv
        setInverse = function(inverse)
                inv<<-inverse
        list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}

## The cacheSolve function will compute the inverse of the function
## If the inverse of the martix is already computed it will return the matrix from cache
## If the inverse of the matrix is not already computed it will compute the inverse of the matrix in setInverse function
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Inverse already computed. Returning matrix from cache")
                return(inverse)
        }
            
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
