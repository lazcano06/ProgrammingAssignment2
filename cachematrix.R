## Create two functions that can cache the inverse of a matrix 
## This function creates a list containing functions to: 
##set a matrix 
#get a matrix 
##set the inverse of the matrix 
##get the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) { 
     m <- NULL ##m is used to store the inverse to cache 
      
     set <- function(y){ 
         x <<- y ##Assigns a matrix to y 
         m <<- NULL ##Inverse in cache is set to NULL 
     } 
      
     get <- function() x ##Shows the matrix function 
     setInverse <- function(inverse) m <<- inverse ##Sets the inverse to m to store it to the cache 
     getInverse <- function() m ##shows the inverse of the matrix saved in cache 
      
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##The variables are stored in a list 
 } 
 

 

## This function shows the inverse of a matrix and if the inverse has already been calculated, the cache will show the matrix 


 cacheSolve <- function(x, ...) { 
     ## Return a matrix that is the inverse of 'x' 
      
     m <- x$getInverse() ##assigns the inverse of a matrix to m 
      
     ##If there's an inverse matrix already, show a message and return the value of m 
     if (!is.null(m)) { 
         message("Getting cache data") 
        return(m) 
     } 
      
     data <- x$get() ## get the matrix without the inverse 
     m <- solve(data, ...) ##calculates the inverse of the matrix 
     x$setInverse(m) ##sets the inverse of m 
    m 
} 
