# The first function, makeCacheMtrix creates a special "vector", 
# which is really a list containing a set of functions to
# 
# set the value of the passed in matrix
# get the value of the passed in matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(matrixObj = matrix()) {
        inv <- NULL
        # set the value of the matrix
        set <- function(y) {
                matrixObj <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() matrixObj
        # set the inverse of the matrix from cache
        setinverse <- function(inverse) inv <<- inverse
        # get the inverse of the matrix
        getinverse <- function() inv
        # create a list containing function references to
        # use in the subsequent function cacheSolve
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(matrixObj, ...) {
        #first checks to see if the inverse has already been calculated.
        inverse <- matrixObj$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        #calculate the inverse of the matrix
        matrix <- matrixObj$get()
        #calculate the inverse of the matrix
        inverse <- solve(matrix)
        #set the resultant inverse matrix in the cache
        matrixObj$setinv(inverse)
        #return the inverse matrix result
        inverse
}
