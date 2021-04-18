## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   im<-matrix()
   set<-function(y){
           x<<-y
           im<<-matrix()
   }
   get<-function()x
   setmat<-function(inversemat)im<<-inversemat
   getmat<-function()im
   list(set=set,get=get,setmat=setmat,getmat=getmat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im<-x$getmat()
        if(!is.na(im)){
                message("getting cached inverse matrix")
                return(im)
        }
        data<-x$get()
        im<-solve(data,...)
        x$setmat(im)
        im
}
