## The following functions are used to create a specific object 
## that stores a matrix and caches its inversion. 

## The function makeCacheMatrix() creates a certain matrix, 
## which is actually a list containing the following four functions:
  
## 1 sets the value of the matrix;
## 2 gets the value of the matrix;
## 3 sets the value of the inverse;
## 4 gets the value of the inverse.
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

## The cacheSolve() function calculates the inverted value of the certain matrix 
## returned by makeCacheMatrix(). If the inverse has already been calculated 
## (and the matrix has not changed), cacheSolve() should fetch the inverse from 
## the cache.
cacheSolve <- function(x, ... ) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("receive cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



## Testing 
T1 <- matrix(c(1,2,3,4), 2, 2)
T2 <- makeCacheMatrix(T1)
cacheSolve(T2)

