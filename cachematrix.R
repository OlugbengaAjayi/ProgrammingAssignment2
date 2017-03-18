## Caching the Inverse of a Matrixless Matrix inversion is usually a costly computation

##As this process is repititive it could be beneficial to caching the inverse rather than computing it every time the function is call


##The goal of MakeCacheMatrix is to
##set the value of the matrix 
##get the vaule of the matrix
##set/compute the value of the matrix 
##get the value of the matrix


##MakeCacheMatrix This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <-function()x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}



##cachesolveThe function computes the inverse of the special matrix returned by the MakeCacheMatrix function, but first the function checks to see if the inverse is present or not, if its absent it computes the inverse and sets the value of the cache.



cachesolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}