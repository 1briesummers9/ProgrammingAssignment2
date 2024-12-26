makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
# Creating a special matrix object
special_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Computing and caching the inverse
inverse_matrix <- cacheSolve(special_matrix)
print(inverse_matrix)

#cached inverse
cached_inverse <- cacheSolve(special_matrix)
print(cached_inverse)
