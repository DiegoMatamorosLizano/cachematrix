## Assignment 2: Lexical Scoping

## Function to create a matrix object able to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  get <- function() x
  set <- function(z) {
    x <<- z
    inver <<- NULL
  }
  get_inver <- function() inver
  set_inver <- function(inverse) {
    inver <<- inverse
  }
  return(
    list(get = get,
         set = set,
         get_inverse = get_inver,
         set_inverse = set_inver)
  )
}


## Function to get the inverse of the matrix created
## on function makeCacheMatrix
## Results of the inverse should be the same if were calculated,
## returning result from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if (!is.null(inverse)) {
          return(inverse)
        }
        i <- solve(x$get())
        x$set_inverse(i)
}
