## These functions will take a matrix, invert it and store the inverted matrix in the cache.
## When the inverted matrix is later called it will be taken from the cached value instead of recalculating it.

## This function creates a list of functions concerning matrix m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##This line sets m to an empty variable.
        set <- function(y) { ##We start a function called set, taking variable y
                x <<- y ##This assigns the value of y to x in the makeCacheMatrix environment
                m <<- NULL ##This assigns a null value to m in the makeCacheMatrix environment
        }
        get <- function() x ##Get is a now a function that returns matrix x
        setinverse <- function(inverse) m <<- inverse  ##This function will take the inverse of a matrix and store it as m in the cache
        getinverse <- function() m ##This function will return the cached inverse of the matrix
        list(set = set, get = get, ##This allows one to use makeCacheMatrix as a list containing the different functions assigned above.
             setminverse = setinverse,
             getinverse = getinverse)
}


## This function will attempt to get the cached value of the inverted matrix from the makeCacheMatrix function.
## If there is no cached value then it will invert the matrix, store that value in the cache and return the inverted matrix.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ##Will take the cached inverted matrix from makeCacheMatrix
        if(!is.null(m)) { ##If the cached matrix exists...
                message("getting cached data") ##Displays a message to say it is taking from the cache
                return(m) ##Returns the cached matrix
        }
        data <- x$get() ##This happens if there is no cached matrix. It gets the matrix assigned to makeCacheMatrix
        m <- solve(data, ...) ##This inverts the matrix
        x$setinverse(m) ##This takes the inverted matrix and stores it in set inverse environment
        m ##The inverted matrix is returned.
}
