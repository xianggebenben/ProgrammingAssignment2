## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

 ## makeCacheMatrix: a list of four functions: 
## set:set the value of the matrix
## get:2.get the value of the matrix
## setinvert:set the value of the inverse matrix
## getinvert:get the value of the inverse matrix

 makeCacheMatrix <- function(x = matrix()) { 
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinvert <- function(invert) m <<- invert
   getinvert <- function() m
   list(set = set, get = get,
        setinvert = setinvert,
        getinvert = getinvert)
  
 } 


 ##cacheSolve: compute the inverse of the matrix using  makeCacheMatrix


 cacheSolve <- function(x, ...) { 
           ## Return a matrix that is the inverse of 'x' 
   m <- x$getinvert()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinvert(m)
   m
   } 

