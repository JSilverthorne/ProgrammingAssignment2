## makeCacheMatrix and cacheSolve store a numeric vector and cache its
## inverse to reduce computation resources

## Create list, initialize to NULL
## Sets value of matrix (set)
## gets value of matrix (get)
## sets inverse of matrix (setinverse)
## gets inverse of matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) i <<- inverse
                getinverse <- function() i
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)

}


## Calculate and return inverse of the matrix.
## If inverse (i) has not been computed, it computes the inverse
## If inverse (i) has been computed, it returns the inverse

cacheSolve <- function(x, ...) {
                i <- x$getinverse()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
}