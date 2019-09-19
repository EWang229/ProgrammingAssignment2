## My function first 1. sets value of the matrix, 2. gets the value of matrix, 3. sets the value of the inverse matrix,
## and 4. gets the value of the inverse matrix. 

## Basically, the function allows the matrix's inverse to be cached. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function calculates the inverse of the matrix, and if it is already calculated, then it gets the value from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
