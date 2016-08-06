# In this assignment, I create a pair of functions to facilitate caching the inverse of a matrix.

#' The first function, makeCacheMatrix, creates a list containing 4 functions to:
#' 1.set the value of the matrix (Use this function only if you want to change the value of the 
#' given matrix.)
# 2.get the value of the matrix
# 3.set the inverse of the matrix (The inverse set here is not necessarily correct.)
# 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    set_inv <- function(inv_input) inv <<- inv_input
    get_inv <- function() inv
    
    list(set = set,
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


#' The following function calculates the inverse of the matrix created with the above function. 
#' However, it first checks to see if the inverse has already been calculated. If so, it gets 
#' the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of 
#' the matrix and sets the value of the inverse in the cache via the set_inv function.
cacheSolve <- function(x, ...) {

    if (!is.null(x$get_inv())) {
        inverse <- x$get_inv()
        message("getting cached inverse of the matrix")
    }
    
    else {
        inverse <- solve(x$get())
        x$set_inv(inverse)
        message("calculating the inverse of the matrix as")
    }
    
    ## Return a matrix that is the inverse of 'x'
    return(inverse)
}



# Example 1
x <- makeCacheMatrix(matrix(c(3,4,2,8),2,2))
cacheSolve(x)

# Example 2
x <- makeCacheMatrix(matrix(c(3,4,2,8),2,2))
x$set(matrix(c(7,34,2,5, 3:-1), 3, 3))
cacheSolve(x)

# Example 3
x <- makeCacheMatrix(matrix(c(3,4,2,8),2,2))
x$set_inv(matrix(c(7,34,2,5, 3:-1), 3, 3))
cacheSolve(x)










