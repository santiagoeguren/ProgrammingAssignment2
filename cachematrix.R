##Function that takes on a matrix as an argument and prepares a list of functions
##defined within it. It can calculate the inverse of the matrix when one calls
##the specific part of makeCachematrix and stores it

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
            set <- function(y) {
            x <<- y
            m <<- NULL
            }
            get <- function() x
            setreverse <- function(reverse) m <<- reverse ##  solver not Solve
            getreverse <- function() m   
            list(set = set, get = get,
            setreverse = setreverse,
            getreverse = getreverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       m <- x$getreverse()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
       data <- x$get()
       m <- solve(data, ...)
       x$setreverse(m)
       m
}
