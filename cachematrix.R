## Collection of functions that implement cached calculation
## of the inverse of a matrix
## 

## This function instantiates a matrix object (list)
## which contains a member to store its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## We initialize the inverse to null
    m <- NULL
    
    # Set a new value to the Matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Retrieves Matrix Value
    get <- function() x
    
    # Sets a new cached inverse Matrix
    setinverse <- function(inverse) m <<- inverse
    
    # Retrieves the cached value of the inversee matrix
    getinverse <- function() m
    
    # Construction of the object using a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function calculates the inverse of a Matrix
## and stores the value in the matrix object

cacheSolve <- function(x, ...) {
    # Gets the stored value of the inverse
    m <- x$getinverse()
    # If the inverse has been previously calculated, returns it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Otherwise it calculates the inverse and stores the new value
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
