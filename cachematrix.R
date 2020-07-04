## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() is a function that contains the setInverse() and getInverse() function 
## to store and retrieve the inverse matrix of the input matrix. 

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inv <<- Inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## The cacheSolve() function takes the makeCacheMatrix() object as an input and solves for and set() the inverse matrix of x
## if the inverse matrix has not been solved for or if a new matrix is set(). Otherwise, the cached inversed matrix will be retrieved. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message('getting cache data')
        return(inv)
    }
    mat <- x$get()
    inv <- 1/(mat[1,1]*mat[2,2] - mat[1,2]*mat[2,1]) * matrix(c(mat[2,2], -mat[2,1], -mat[1,2], mat[1,1]))
    x$setInverse(inv)
    inv
}

# Test case: 
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# myMatrix <- makeCacheMatrix(m1)
# cacheSolve(myMatrix)

