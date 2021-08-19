## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
  # function takes in a matrix
  i<- NULL #initalise the inverse property
  
  
  set<- function(matrix){
        x<<- matrix
        i<<- NULL
  }
  get <- function() {
        x
  } #get the matrix and return the matrix
  
  #set the inverse of the matrix
  
  setinverse <- function(inverse){
    i<<- inverse
  }
  #get the inverse of the matrix
  
  getinverse <- function() {
    i
  }
  
  #return a list of the methods
  list(set = set,
       get = get,
       setinverse =setinverse,
       getinverse = getinverse)
      
}


## Write a short comment describing this function
## this function computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # return inverse if its already set
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data<- x$get() # get matrix from our object
        i<- solve(data, ...) # calculate the inverse
        x$setinverse(i) # set the inverse to object
        i # return the matrix
}
