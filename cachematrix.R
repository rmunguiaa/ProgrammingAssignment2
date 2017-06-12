## Put in Cache Memory the Inverse of a Matrix:
## I took the makeVector and cachemean and change the function mean() 
## by solve() which is used to get a inverse matrix
## the program has two functions makeCacheMatrix and cacheSolve

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv

        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## Function to get the inverse of matrix x created by the above procedure. 
## If the inverse is the cache and the matrix has not changed, the inverse will 
## be retrieve from the cache

cacheSolve <- function(x, ...) {
        
inv <- x$getInverse()  ## change here respect a cachemean
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        matrixx <- x$get()
        inv <- solve(matrixx, ...)  ## change here respect a cachemean
        x$setInverse(inv)
        inv
}
