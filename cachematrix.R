## This file contains 2 functions makeCacheMatrix that create a list containing a
## getters and setters to a matrix and its inverse and cacheSolve that checks if a
## given matrix has already a calculated inverse, if yes it takes it from the
## cache otherwise it calculate it and return it back


## defines setters and getters for Matrix and its inverse, it takes as a parameter
## a matrix we assume that all the provided matrices can be inverted
makeCacheMatrix <- function(mtx = matrix()) {
    inverseMtx <- NULL
    set <- function(initMtx) {
        mtx <<- initMtx
        inverseMtx <<- NULL
    }
    get <- function() mtx
    setInverse <- function(invMtx) inverseMtx <<- invMtx
    getInverse <- function() inverseMtx
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve takes a matrix container that corresponds to the list with getters
## and setters returned by makeCacheMatrix it checks if the inverse matrix exists
## if yes it returns it from the cache otherwise it calculates the inverse Matrix
## using solve function

cacheSolve <- function(mtxContainer, ...) {
    ## first check for the inverse of the matrix is it is not NULL
    inverseMtx <- mtxContainer$getInverse()
    if (!is.null(inverseMtx)) {
        message("getting the cached inverse of the matrix")
        return(inverseMtx)
    }
    ## else calculate the inverse of the matrix
    mtx <- mtxContainer$get()
    inverseMtx <- solve(mtx)
    mtxContainer$setInverse(inverseMtx)
    message("calculating the inverse of the matrix")
    ## Return a matrix that is the inverse of 'mtx'
    inverseMtx
}
