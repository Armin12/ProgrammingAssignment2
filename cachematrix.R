# pair of functions that cache the inverse of a matrix


#makeCacheMatrix function: creates a special 
#"matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_val <- NULL
  set <- function(y) {
    x <<- y
    inv_val <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_val <<- inv #cash matrix inverse
  getinv <- function() inv_val
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}






#cacheSolve: computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the 
#cache. solve(X) calculates inverse of matrix X


cacheSolve <- function(x, ...) {
  inv_val <- x$getinv()
  if(!is.null(inv_val)) {
    message("getting cached data")
    return(inv_val)
  }
  data <- x$get()
  inv_val <- solve(data, ...) #Computing the inverse of a square matrix
  x$setinv(inv_val)
  inv_val
}

