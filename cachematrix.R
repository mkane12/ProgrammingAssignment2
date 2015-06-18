## makeCacheMatrix caches an input (invertible) matrix x as well as
## its inverse i. This function contains the functions set (which will
## change the value of the matrix), get (which will return the matrix),
## setinverse (which will change the value of the matrix's inverse), and
## getinverse (which will return the value currently stored as i).
##
## cacheSolve will return the current value of i if one exists. If not,
## it will solve the input (invertible) matrix x and cache the
## calculated inverse.

## This function takes a matrix x as input and caches its inverse

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


## This function computes the inverse of the matrix x inputted by the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}
