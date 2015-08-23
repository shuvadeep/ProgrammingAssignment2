
## This is my function which includes
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
inver <- NULL 
set <- function(y) {
x <<- y
inver <<- NULL
}
get <- function() x
setinversematrix <- function(inverse) 
inver <<- inverse
getinversematrix <- function() inver
list(set = set,
get = get,
setinversematrix = setinversematrix,
getinversematrix = getinversematrix)
}
## the above function makeCacheMatrix computes the inverse of the special "matrix"
#If the inverse has already been calculated (and the matrix has not changed),
#then it should retrieve the inverse from the cache.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inver <- x$getinversematrix()
if (!is.null(inver)) {
message("getting cached data")
return(inver)
}
mat <- x$get()
inver <- solve(mat, ...)
x$setinversematrix(inver)
inver
}
#this will return the expected inverse of 'x'