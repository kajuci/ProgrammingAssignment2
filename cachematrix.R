## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  ##Set the matrix
  set<-function(matrix){
    x <<- matrix
    m <<- NULL
  }
  ##Get the matrix
  get<-function(){
    x
  }
  ##Set inverse matrix
  setInv<-function(value){
    m<<- value
  }
  ##Get inverse matrix
  getInv<-function(){    
    m
  }
  ##list of methods
  list(set=set, get=get,
       setInv = setInv,
       getInv = getInv)  
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getnv()##Inverse of x
 
  if(!is.null(m)){
    message("getting data")
    return(m) ##Return inverse Matrix if was set
  }
  
  matr <- x$get()##Get the matrix
  m <- solve(matr) %*% matr ##Calculate inverse
  x$setInv(m)##Inverse to the object
  m ##Matrix 
  
}
