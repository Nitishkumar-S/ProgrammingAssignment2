## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #Creating a null matrix
        im<-matrix()
   
        #function for intialising new input matrix if makeCacheMatrix() object is already created
        set<-function(y){
                x<<-y
                im<<-matrix()
        }
        
        #function to retrieve the values of input matrix
        get<-function(){
                x   #returning input matrix
        }
        
        #function to initialize im ,so that it can used from cache later
        setmatrix<-function(inversematrix){
                im<<-inversematrix      #im is assigned the inversematrix values
        }
        
        #function to retrieve the value of im 
        getmatrix<-function(){
                im
        }
        
        #Below list is returned and contains 4 objects in it
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #retrieving the value of cached inverse matrix
        im<-x$getmatrix()
        
        #checks if there is any cached value present
        if(!is.na(im)){
                
                message("getting cached inverse matrix")
                return(im)           #returning the cached value
                
        }
        
        #retrieving the input matrix
        data<-x$get()
        
        #finding the inverse of matrix stored in "data" using "solve()"
        im<-solve(data,...)
        
        #Assigning value to im so it remains in cache for future use 
        x$setmatrix(im)
        
        #returning the inverse matrix computed
        im
}
