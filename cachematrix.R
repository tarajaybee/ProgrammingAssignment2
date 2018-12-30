## makeCacheMatrix and cacheSolve together take as input an invertible square matrix, 
##and calculate, cache, and return its inverse. 

##makeCacheMatrix takes a square matrix and stores the appropriate functions to retrieve the 
##matrix, set its inverse, and retrieve its inverse (if already stored)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##cacheSolve takes the list of functions with the matrix data created from makeCacheMatrix 
##and checks if the inverse of the matrix has already been stored & retrieves this information. 
##If the inverse has not been cached, it solves the inverse matrix of the data and both stores
##& returns this information.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
