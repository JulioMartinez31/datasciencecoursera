## Put comments here that give an overall description of what your
## functions do

library(matlib)

makeCacheMatrix <- function(x = matrix()){
  if (ncol(x)==nrow(x) && det(x)!=0) {
    m <- NULL
    set<-function(y){
      x <<- y
      m <<- NULL
    }
    get<-function() x
    setinverse <- function() m <<- solve(x)
    getinverse<-function() m
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
    
  }else{
    return(message("This matrix is not invertible"))
  }
}

makeCacheMatrix <- function(x = matrix()) {

## this function find the inverse of a matrix
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
}



#####  Example 

m<-makeCacheMatrix(matrix(c(1,2,1,2,2,1,3,1,2),ncol=3,byrow=TRUE))
m$get() # get the matrix 'm'
m$setinverse()   # Set inverse of a matrix 'm'
m$getinverse()   ## Return a matrix that is the inverse of 'm'

