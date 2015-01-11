#makeCacheMatrix: This function creates a special "matrix" object
#that can cache its inverse.

#cacheSolve: This function computes the inverse of the special
#"matrix" returned by makeCacheMatrix above. If the inverse
#has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
     c <- NULL
     set <- function(y) {
          x <<- y
          c <<- NULL
     }
     get <- function() x
     setcache <- function(solve) c <<-solve
     getcache <- function() c
     list(set = set, get = get,
          setcache = setcache,
          getcache = getcache)
}
## Creates a special "matrix" object that can cache its inverse

cacheSolve <- function(x, ...) {
     c <- x$getcache()
     if(!is.null(c)) {
           message("inverse cached")
           return (c)
     }
     data <- x$get()
     c <- solve(data)
     x$setcache(c)
     c
}
## Returns a matrix that is the inverse of 'x'