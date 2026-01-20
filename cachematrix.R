## Put comments here that give an overall description of what your
## functions do
##  creates a special "matrix" object that can cache its inverse.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #cached inverse, start empty
  set<-function(y){
    x<<-y
    inv<<-NULL # matrix changed, invalidate cache
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(
    set=set,
    get=get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function
##computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
        ## Return a matrix that is the inverse of 'x'
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
}
