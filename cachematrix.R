## In order to improve the performance of our code, as calculating the inverse of a matrix can be really 
## complex in terms of computation time, two functions are created to cache the calculation of the inverse.

## makeCacheMatrix creates a list of functions, to manipulate the content of a matrix and its inverse.
## In particular, the list contains the following functions:
## set: to store the the data of the matrix
## get: to get the data of the cached matrix
## setinv: to store the data of the calculated inverse of the matrix
## getinv: to obtain the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(invmatrix) inv <<- invmatrix
  getinv<-function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Return a matrix that is the inverse of 'x', assuming it is always invertible.
## If the inverse matrix already exists, we returned it
## If not, we calculate it (with solve function), and cache it for future calculations
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}
