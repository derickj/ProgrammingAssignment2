## R Programming Course: rprog-14
##
## Programming Assignment 2
## The file contains two functions, the first creates a "matrix" object from
## an invertible matrix (input paramater), the second calculates the inverse
## of the matrix (or returns the cached inverse if already calculated)
##
## Example usage: 
##    m <- rbind(c(1,-1/4), c(-1/4,1))
##    mv <- makeCacheMatrix(m)
##    cacheSolve(mv)   
##    cacheSolve(mv)   
##    mv$set(rbind(c(1,-1/4), c(-1/4,1)))
##    cacheSolve(mv)   

## 
## makeCacheMatrix(matr)
## Arguments
##    x - an invertible matrix
## Return
##    The function returns a new object, which is actually a vector
##    containing 4 functions:
##      set is used to set() the object (only needed when it changes)
##      get is used to get() the original matrix (the original input)
##      setinv is used to set the matrix inverse (but in the closure of the
##         makeCacheMatrix function,i.e. it would change the inv value
##      getinv simply return the matrix inverse stored in the object
##         (NULL if this is the first call for the object in question)
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinv <- function(invx) inv<<-invx
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## 
## cacheSolve(vect)
## Arguments
##    x - a special vector created by makeCacheMatrix()
## Return
##    Returns a matrix that is the inverse of the matrix
##    from which x was constructed using makeCacheMatix
##
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        ## The inverse was already calculated and is simply returned
        message("getting cached inverse")
        return(inv)
    }
    ## The inverse has not yet been calculated
    data <- x$get()          ## Retrieve the original (input) matrix
    inv <- solve(data)       ## Calculate the inverse
    x$setinv(inv)            ## Update the inverse for the vector
    inv                      ## Return the inverse
}
