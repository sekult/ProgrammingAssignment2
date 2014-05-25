## Below are two functions that create a special "matrix" object
## and cache its inverse. Note, this assignment assumes that the
## matrix supplied is always a square invertible matrix.

## The first function, makeCacheMatrix, creates a special "matrix"
## object which can cache its inverse.
## This is done with a list containing functions to:
## 1. set the data in the matrix;
## 2. get the data in the matrix;
## 3. set the data for the inverse matrix; and
## 4. get the data for the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The second function, cacheSolve, calculates the inverse
## of the special "matrix" created with the above function.
## However, first it checks to see if the inverse has already
## been created. If so, it returns the cached result. Otherwise,
## it calculates the inverse of the data and updates the cached
## data via the setInverse function.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
