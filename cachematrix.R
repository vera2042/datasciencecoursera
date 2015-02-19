## The function makeCasheMatrix takes a matrix as input
## and screates a special "vector", which is a list 
## containing a function to 
##
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve caclulcates the inverse of the the special
## "vector" created with the above function. 
## However, it first checks to see if the inverse has already been 
## calculated. If so, it `get`s the mean from the cache 
## and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cashed data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
