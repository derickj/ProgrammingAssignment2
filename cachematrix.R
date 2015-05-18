## R Programming Course rprog-14
## Programming Assignment 2
## The file contains two functions, the first creates a "matrix" object from
## an invertible matrix (input paramater), the second calculates the inverse
## of the matrix (or returns the cache value if already calculated)
##
## Example usage: 
##    mymatrx <- rbind(c(1,-1/4), c(-1/4,1))
##    myspecialmatrix <- makeCacheMatrix(mymatrx)
##    cacheSolve(myspecialmatrx)
## 

## 
## makeCacheMatrix(matr)
## Arguments
##    matr - an invertible matrix
## Returns
##    The function returns a new object, which is actually a vector
##    containing 4 functions:
##      set is used to set() (initialize) the object
##      get is used to get() the original matrix (the original input)
##      setinv is used to set the matrix inverse (but in the closure of the
##         object created by the function
##      getinv simply return the matrx inverse stored in the object
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
## cacheSolve(specialmatr)
## Arguments
##    specialmatr - an special "matrix" or vector object created by makeCacheMatrix
## Returns
##    The function returns the inverse of the matrix. 
##    If this is the first time cacheSolve is called for the specific object
##    the inverse is calculated using the solve() function and then setinv
##    is used on the object to cache the inverse value
##    On subsequent calls, the inverse is merely returned as it has already 
##    been calculated on the first call for a particular object.
##


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("cached")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
