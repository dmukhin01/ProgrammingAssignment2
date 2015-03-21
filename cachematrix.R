## makeCacheMatrix creates an object with four methods for:
## setting a matrix
## getting a matrix,
## setting its inverse, and
## getting its inverse
## 
## cacheSolve calculates the inverse of a given invertible matrix,
## but only if it has not been yet calculated,
## otherwise it uses the previously calculated inverse

## A function for setting and getting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(whatever) {
                inverse <<- whatever
        }
        
        getInverse <- function() {
                inverse
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve checks if the inverse of the input matrix
## has already been calculated.
## If it has been, it is used without recomputing,
## otherwise the matrix is checked for invertibility, and
## if it is invertible, then its inverse is computed and returned

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        if(det(data)==0) {
                message("the matrix is not invertible")
                return(NULL)
        } else {
                inv <- solve(data)
                x$setInverse(inv)
                inv
        }
}