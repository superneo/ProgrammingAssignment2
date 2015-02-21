## Here are a pair of functions that cache the inverse of a matrix.
## More details will come along with each function as following.


## This function creates a special "matrix" object that can cache its inverse.
## It’s assumed that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        if(all(dim(x) == dim(y))) {
            ## if both x and y have the same dimension
            if(!all(x == y)) {
                ## but not all elements in both are equal, respectively
                x <<- y
                inv <<- solve(y)
            }
        }
        else {
            ## if x and y have different dimensions
            x <<- y
            inv <<- solve(y)
        }
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then this function
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    ## Calculate and cache the inverse matrix
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
