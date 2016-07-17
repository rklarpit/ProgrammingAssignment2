## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This funtion will define all the functions necessary to work on
##inverting the matrix in the cacheSolve Function

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    seti <- function(inverse) i <<- inverse
    geti <- function() i
    list(set = set, get = get,
         seti = seti,
         geti = geti)
}


## Write a short comment describing this function
## This function will determine whether the matrix has already been
##inverted or not. If not then the appropriate funtion is called which
##are defined in the makeCacheMatrix function and then use solve
##function to invert the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$geti()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$seti(i)
    i
}
