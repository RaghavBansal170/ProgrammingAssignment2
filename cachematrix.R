## The function makeCacheMatrix takes a matrix as its input, set the value of the matrix,
## gets the value of the matrix, set the inverse matrix and gets the value of the inverse matrix.
## It also caches i.e stores the the value of the inverse matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
         get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function takes the output of the above function makeCacheMatrix as its input
## and sees if the inverse matrix from makeCacheMatrix contains any value or not.
## If it's empty then it gets the original data and set inverse of matrix from the solve function.
## Otherwise, if it isn't empty then it returns a message with the cached data.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                 message("getting cached inverse matrix")
                return(inv)
                }
                
                mat <- x$get()
                inv <- solve(mat, ...)
                x$setinverse(inv)
                return(inv)
}
