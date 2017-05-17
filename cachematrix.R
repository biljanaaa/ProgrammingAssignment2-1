## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly 
## Two functions, makeCacheMatrix and cacheSolveare can be used to cash the matrix inversion 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse 
## that is actually a list of following functions:

makeCacheMatrix <- function(x = matrix()) {
  
  inverseX <- NULL
  # set function sets the value of the matrix  
  set<- function(y)
  {x<<-y
  inverseX <<- NULL
  }
  # get function gets the value of the matrix  
  get <- function(){x}
  # setInverse function sets the value of the inverse matrix   
  setInverse <- function(invrMatr){
    inverseX<<-invrMatr
  }
  # getInverse function gets the value of the inverse matrix   
  getInverse<-function(){inverseX}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        
## Return a matrix that is the inverse of 'x'
  inverseX <-x$getInverse() 
  if(!is.null(inverseX)) {
    message("From cached data")
    return(inverseX)
  }
  data <- x$get()
  inverseX <- solve(data)
  x$setInverse(inverseX)
  return (inverseX)
}

## simple test with matrix that has inverse
##
## a1<-matrix(c(1,1,1,3,4,3,3,3,4),3,3)
## a1c<-makeCacheMatrix(a1)
## 
## cacheSolve(a1c) #display inverse from cashed data
##  
## a1%*%cacheSolve(a1c)

