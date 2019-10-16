## Function makeCacheMatrix will create a matrix object that can cache the inverse


makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  Setinv <- function(inverse) m_inv <<- inverse
  Getinv <- function() m_inv
  list(set = set,
       get = get,
       Setinv = Setinv,
       Getinv = Getinv)
}


## Below is the function to compute the inverse of the matrix created by above function.
## We can now retrieve the inverse from the cache if it is already calculated. 

cacheSolve <- function(x, ...) {
  m_inv <- x$Getinv()
  if (!is.null(m_inv)) {
    message("Data from Cache")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$Setinv(m_inv)
  m_inv
}
