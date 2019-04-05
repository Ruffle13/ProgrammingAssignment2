##The first function will take a matrix as an argument and return a list containing four functions. 
##The second function will take one such list and use functions stored within it to either retreive the inverse
##or retreive the matrix, solve it, and store it. 

## Takes a matrix and makes a list (of functions) allowing for retreival of the matrix 
##and its inverse, as well as the ability to set the inverse or matrix itself. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a list made from the above function as an argument. Uses the list's getinverse function to return the stored value of the inverse
## If the value isn't NULL, it is returned. If it is NULL, gets the original matrix, solves it, and stores it in the list for later retreival.  


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

q <- makeCacheMatrix(matrix(1:4, nrow=2))
print (cacheSolve(q))
print (cacheSolve(q))
t <- makeCacheMatrix(matrix(c(1,2,2,1), nrow=2))
print (cacheSolve(t))
print (cacheSolve(q))
print (cacheSolve(t))






