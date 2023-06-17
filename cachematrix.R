## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    a <- null 
    set <- function(y){
        x <- -y
        a <- null
    }
    get <- function(x){
        set_inverse <- function(inverse) a <- inverse
        get_inverse <- function() a 
    
    list(set=set, get=get,
         setsolve=set_inverse,
         getsolve=get_inverse)
        
    }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.na(inv)){
        message("get cached data")
        return(inv)
    }
    matrix_invert <- x$get()
    inv <- solve(matrix_invert, ...)
    x$setinverse(inv)
    inv
}

