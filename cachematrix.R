## Put comments here that give an overall description of what your
## functions do

## Function that sets and gets value of the matrix, sets the value of the matrix inverse, and gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setvalue <- function(y){
    x<<-y
    inv<<-NULL
  }
  getvalue<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(setvalue=setvalue,getvalue=getvalue,setInverse=setInverse,getInverse=getInverse)

}


## Function that computes the inverse of the matrix returned by previous function or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
