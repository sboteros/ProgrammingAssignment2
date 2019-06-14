# Programming assignment 2: Lexical scoping
# R Programming - Data Science Specialization
# Santiago Botero Sierra
# sboteros@unal.edu.co
# Encoding: UTF-8

# `makeCacheMatrix` returns a list of functions. The first one, `set` assigns
# its argument to a spetial matrix, and has no inverse yet; the second one,
# `get` retrieves its argument; next, `setinverse` assigns a value to the
# inverse matrix `i`; finally, `getinverse` retrieves the last matrix.

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

# `cacheSolve` takes the list from the previos function and verifies if the
# inverse matrix has already been calculated. If so, it returns it; otherwise,
# it uses the function `get` to sed the data, computes its inverse and uses the
# function `setinverse` to set it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}