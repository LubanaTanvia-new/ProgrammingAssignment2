## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The function 'makeCacheMatrix' creates a special 'matrix' object and is a list of
#functions to firstly set the value of the matrix, get the value of the matrix, afterthat 
# set the value of the inverse, get the value of the inverse. Also the
#matrix object can cache its inverse.

##the <<- operator which can be used to assign a value to an object in
#an environment that is different from the current environment.


makeCacheMatrix <- function(x = matrix()) {
  invs_Mat<-NULL
  #set the value of the 'matrix'
  set_Mat<-function(y){               
    x<<-y
    invs_Mat<-NULL
  }
  #get the value of the 'matrix'
  get_Mat<-function() x
  #set the value of the matrix which is invertible
  set_Invs<- function(inverse) invs_Mat<<- inverse
  #get the value of the matrix which is invertible
  get_Invs<- function() invs_Mat
  list(set_Mat=set_Mat, get_Mat=get_Mat, set_Invs=set_Invs, get_Invs=get_Invs)
}


## Write a short comment describing this function

##The function 'cacheSolve' takes the output returned by 'makeCacheMatrix' as an 
#input and computes the inverse. However, it firstly checks whether inverse has 
#been computed or not. If the inverse matrix obtained from makeCachematrix(matrix)
#is empty, it gets the original matrix from data and use solve function to compute
#inverse. Otherwise, returns a message saying, "GETTING CACHED INVERTIBLE MATRIX!".
#retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get the value of invertible matrix from function 'makecachematrix'
  
  invs_Mat<- x$get_Invs()
  
  #if inverse matrix'invs_Mat' is not empty, type a message and return the invertible matrix'invs_Mat'
  
  if(!is.null(invs_Mat)){
    message("GETTING CACHED INVERTIBLE MATRIX!")
    return(invs_Mat)
  }
  
  #if inverse matrix 'invs_Mat' is empty, get the original matrix, use solve function to get inverse, set the invertible matrix and return it.
  
  mat_Data<- x$get_Mat()
  invs_Mat<-solve(mat_Data, ...)
  x$set_Invs(invs_Mat)
  return(invs_Mat)
}
