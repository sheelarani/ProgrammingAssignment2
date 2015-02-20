## Put comments here that give an overall description of what your
## functions do

##This function is used to cache and retreive matrix of values ##and function. Variables "get","set", "getinverse", setinverse" ## as a "list" are exposed to pass input variable and get cached ##varible and function.

makeCacheMatrix <- function(x = matrix()) {

    # Set variable m to null, m will be used to cache 
    # the output of parent function                                             
    m <- NULL 
  
    # set variables x,m for parent function
    set <- function(y) {
        # set the function input variable to "x" in parent
        # funtion
        x <<- y
            
        # set variable "m" to null in parent function
        m <<- NULL
   }

   # get input variable of this function, which is used in 
   # parent function to get the input variable 
   
   get <- function() x
  
   # set the output of the function for cached variable m,
   # this is used by parent function, to store to cache in m, 
   # after inversing the matrix

  setinverse <- function(inverse) m <<- inverse
  
   # get the output of function set in parent function,
   # the parent function ran this cached function on cached
   # variable "m"

  getinverse <- function() m
 
   # List of variables exposed outside this function, these
   # variable are used to set,get the variable and function 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the input matirx from calling function, 
## checks if the results of matrix inversion are cached, 
## if they are cached then returns the cached output
## otherwise performs the matrix inversion
## and reutrns the output to calling function

cacheSolve <- function(x, ...) {
  # get the cache variable m from calling function,
  # m is not exposed outside of this function, can be only 
  # retrieved from calling function

  m <- x$getinverse()
 
  # check if the cache m is null, means the inverse of the matrix 
  # is not calculatedand stored in m

  if(!is.null(m)) {
    
    message("getting cached data")
    
    # if m is not null, means already cahced, then just return 
    #  the cache
    return(m)
    
  }
    
  # get the input matrix from calling function
  data <- x$get()
  
  # inverse the input matrix using solve and store in cache 
  # varable m

  m <- solve(data, ...)
  
  # set the output to cached varible m, in the calling function

  x$setinverse(m)
  
  # return the final output of this function, which is stored in 
  # cache varible m
  m
}
