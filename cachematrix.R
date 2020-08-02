## Put comments here that give an overall description of what your
## functions do

# Make inverse of matrix and cache
makeCacheMatrix <- function(x = matrix()) {
    inv <<- NULL
    #set value of vector
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get value of vector
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get=get, setinverse=setinverse, getinverse= getinverse)

}

## calculates the inverse of the vector created with the above function, 
## checking if inverse has already been calculated before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    # check if inverse of matrix has already been calculated
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    return(inv)
}