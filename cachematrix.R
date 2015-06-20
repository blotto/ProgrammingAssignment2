## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly.

# see http://www.mathwords.com/i/inverse_of_a_matrix.htm
# Example 1:
#    > A = matrix( c(4,3,3,2), nrow=2, ncol=2)
#    > A
#         [,1] [,2]
#    [1,]    4    3
#    [2,]    3    2
#    > m = makeCacheMatrix(A)
#    > cacheSolve(m)
#         [,1] [,2]
#    [1,]   -2    3
#    [2,]    3   -4
#    > cacheSolve(m)
#    getting cached data
#         [,1] [,2]
#    [1,]   -2    3
#    [2,]    3   -4

# Example 2:
#    > A = matrix( c(1:4), nrow=2, ncol=2)
#    > A
#         [,1] [,2]
#    [1,]    1    3
#    [2,]    2    4
#    > m = makeCacheMatrix(A)
#    > m$get() == A
#         [,1] [,2]
#    [1,] TRUE TRUE
#    [2,] TRUE TRUE
#    > cacheSolve(m)
#         [,1] [,2]
#    [1,]   -2  1.5
#    [2,]    1 -0.5
#    > cacheSolve(m)
#    getting cached data
#         [,1] [,2]
#    [1,]   -2  1.5
#    [2,]    1 -0.5

# Example 3: 3x3 invertible matrix
#    > A = matrix( c(1,2,3,0,4,5,1,0,6), nrow=3, ncol=3)
#    > m = makeCacheMatrix(A)
#    > cacheSolve(m)
#                [,1]       [,2]        [,3]
#    [1,]  1.09090909  0.2272727 -0.18181818
#    [2,] -0.54545455  0.1363636  0.09090909
#    [3,] -0.09090909 -0.2272727  0.18181818
#    > cacheSolve(m)
#    getting cached data
#                [,1]       [,2]        [,3]
#    [1,]  1.09090909  0.2272727 -0.18181818
#    [2,] -0.54545455  0.1363636  0.09090909
#    [3,] -0.09090909 -0.2272727  0.18181818

makeCacheMatrix <- function(x = matrix()) {
    # Defines a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: A matrix, assumes the supplied matrix is a square invertible matrix
    #
    # Returns:
    #   A "matrix" object that can cache its inverse.
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinv<- function(inv) invMatrix <<- inv
    getinv <- function() invMatrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    # Computes the inverse of the matrix object defined by makeCacheMatrix. Takes advantage of caching to improve
    # computational performance.
    #
    # Args:
    #   x: A matrix object defined by makeCacheMatrix
    #
    # Returns:
    #   The inverse of the input matrix
    inverse <- x$getinv()
    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
