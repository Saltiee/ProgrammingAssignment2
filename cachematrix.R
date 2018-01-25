## Caching the Inverse of a Matrix

## This function creates a special "Matrix", which is in fact a list
## containing functions to set and get the value of the matrix, and
## to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y){
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) v <<- inverse
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function returns the inverse of the special "Matrix"
## returned by the above function. The function retrieves the
## inverse directly from the cache and returns a message
## if the inverse has been calculated

cacheSolve <- function(x, ...) {
    v <- x$getinverse()
    if(!is.null(v)){
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...) ## Return the inverse
    x$setinverse(v)
    v
}
