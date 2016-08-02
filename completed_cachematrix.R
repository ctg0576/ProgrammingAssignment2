## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function is similar to the example. 
# I simply rename some functions and variables to make them easier to understand.
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

# Also similar to the example. We first try to get the cached inverse. If exits, then return the value.
# If not, we get the matrix x and calculated the inverse by solve function
# Then we set the calculated value to the cache and return the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

