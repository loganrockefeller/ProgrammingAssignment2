## this function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL 
        setMatrix <- function(y) { 
                x <<- y 
                m <<- NULL 
        }
        getMatrix <- function() x 
        setInverse <- function(solve) m <<- solve 
        getInverse <- function() m 
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,        
             setInverse = setInverse,  
             getInverse = getInverse)  
      }


## this function calculates the inverse of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

