
# This function checks whether the inverse is already cached .In order to not do excess computational work .This helps to limit the work by checking if the inverse is already there or not .
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    
  }
  get<-function() x
  setinverse<-function(inverse) inv <<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}




cacheSolve <- function(x, ...) {
  ##  This function returns the matric that is Inversed. So it returns the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("This is getting cached data")
    return (inv)
  }
  data<-x$get()
  inv<-solve(data,....)
  x$setinverse(inv)
  inv
}
 

