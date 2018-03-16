# makeCacheMatrix creates a special "matrix" object that can cache its inverse,
# which is really a list containing a function to:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the inverse of the matrix
# 4.  get the inverse of the matrix

makeCacheMatrix <- function(my_matrix = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        my_matrix <<- y
        inv_matrix <<- NULL
    }
    get <- function() my_matrix
    setinv <- function(inv) inv_matrix <<- inv
    getinv <- function() inv_matrix
    list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)  
}

# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getinv()
    if(!is.null(inv_matrix)){
        message("getting cached data")
        return(inv_matrix)
    }
    my_matrix <- x$get()
    inv_matrix <- solve(my_matrix, ...)
    x$setinv(inv_matrix)
}
