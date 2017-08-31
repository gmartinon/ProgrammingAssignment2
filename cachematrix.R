# This is kind of a hidden object-oriented programming. The function
# makeCacheMatrix is a class definition improving that of matrix, providing two
# members: x, a matrix; and inv, the invert matrix of x. On top of that, four
# methods are provided, which are the usual getters and setters allowing to
# read/write the members.
#
# When makeCacheMatrix is called with a valid argument, an instance of the class
# is created (let's call that class cache-matrix), and stored in whatever
# variable you assigned to the result, e.g. my_cache_matrix.
#
# cacheSolve is nothing but an improved solve that fits our cache-matrix object.
# Indeed, it calls successively the getters and setters of our cache-matrix
# object in order to compute the invert matrix only when it is not yet computed.

## The makeCacheMatrix function (or cache-matrix class), which needs an ordinary
## matrix to feed the constructor. The invert matrix is by default set to NULL.
## Four methods are provided: get(), set() for reading/writing the matrix member,
## and getinv(), setinv() for reading/writing the invert matrix member.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(value) inv <<- value
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function, which take advantage of the methods of the
## cache-matrix class to compute the invert matrix member only when it is
## set to NULL (and not otherwise)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)
   inv
}
