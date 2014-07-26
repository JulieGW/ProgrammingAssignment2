## 
## This function set will take the inverse of an invertible
## matrix, and store that value in a cache.  
## Given a matrix the first part just sets things up.
## The second part first checks to see if the matrix has
## already been inverted, and then either uses that answer
## if it exists, or calculates it and stores it if it does
## not. 

## Write a short comment describing this function

## This function creates a special "matrix" object that can
## cache its inverse. It does not calculate the inverse.
## It does create a place to put the matrix and its inverted
## value and sets the initial inverted value to NULL

makeCacheMatrix <- function(x = matrix()) {
  f<-nrow(x)
  g<-ncol(x)
  z<-matrix(nrow=f,ncol=g)
  set<-function(y){
    x<<-y
    z<<-matrix(nrow=f,ncol=g)
  }
  get<-function()x
  setinv<-function(flip) z<<-flip
  getinv<-function() z
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This is the code that inversts the matrix and stores 
## the inverted matrix in a cache. If that has already
## happened, instead of the matrix being inverted, it
## pulls out the cached value.

cacheSolve <- function(x, ...) {
         z<-x$getinv()
       if(!is.na(z[1,1])){
              message("getting cached data")
             return (z)
       }
       data<-x$get()
       z<-solve(data)
       x$setinv(z)
       z
   }

