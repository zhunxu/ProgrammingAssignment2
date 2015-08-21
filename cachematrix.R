## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse, 
## which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## create the inverse matrix of the special "matrix" created with the above function
## 1. checks to see if the inverse has already been create. 
##    If so, it gets the inverse from the cache and skips the creation. 
## 2. Otherwise, it inverse the matrix of the data 
##    and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
