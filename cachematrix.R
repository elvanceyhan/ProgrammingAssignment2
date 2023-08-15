## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

#set the elements of the matrix
#get the elements of the matrix
#set the elements of the matrix inverse
#get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    mat.inv <- NULL
    set <- function(y) {
        x <<- y
        mat.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mat.inv <<- inverse
    getinverse <- function() mat.inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#The following function calculates the inverse of the special “matrix” created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat.inv <- x$getinverse()
    if(!is.null(mat.inv)) {
        message("getting cached data")
        return(mat.inv)
    }
    mat2invert <- x$get()
    mat.inv <- solve(mat2invert, ...)
    x$setinverse(mat.inv)
    mat.inv
}
