## Put comments here that give an overall description of what your
## functions do
# These functions provide a caching function for inversion of invertible matrices
# i.e. square matrices. When a matrix is stored with the makeCacheMatrix function, the
# first time that matrix is inverted with the cacheSolve function the result is stored 
# a cache (as well as being returned by the cacheSolve function).

# Subsequent calls to cacheSolve retrieve the inverted matrix from the cache, reducing 
# unnecessary calculations (i.e. avoiding the inversion calculation)

# The cache makes use of superassignment - in effect storing the objects in the 
# parent (enclosing) environment of the function, which will be the 
# environment that is created when the initial call to makeCacheMatrix is assigned to a 
# variable. 

# As the environment is different each time you call makeCacheMatrix to store different 
# matrices (where you assign them to different objects) makeCacheMatrix will create a separate cache for each.


## Write a short comment describing this function
# This function creates a list of functions for storing and retrieving both a user-supplied
# matrix and its inverse in a cache. It is to be used with the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
  
  # set the local variable used to hold the inverse to null (reinitialize it)
  # whenever makeCacheMatrix is called
  m <- NULL
  
  # This function will set x in its parent environment
  # to the parameter y (the original matrix passed to the 
  # function), and sets m in its parent environment (which will
  # later store the inverse) to be null.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # This function returns the original matrix,
  # retrieving the value from its parent environment
  get <- function() x
  
  # This function set the value of m in its parent 
  # environment to be the value you pass it as inverse
  # It only gets called by cacheSolve the first time 
  # cacheSolve is called for your matrix (in subsequent
  # calls cacheSolve will get the value from the cache
  # and return)
  setinverse <- function(inverse) m <<- inverse
  
  # This function retrieves the value of the inverse from
  # it's parent environment (any time cacheSolve is called
  # after the first time)
  getinverse <- function() m
  
  # Used by calling functions to see what functions are in makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
# This function is called with an argmuent which is the object created
# by a prior call to createCacheMatrix (where a matrix to be inverted is
# stored in its cache). It determines if the inverse of that matrix is 
# present in the cache and if so, returns it. If not, it calculates the
# inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Retrieves the value of the inverse stored in the 
  # cache (if any)
  m <- x$getinverse()
  
  # If there was an inverse stored in the cache, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If no inverse was found in the cache:
  # Retrieve the original matrix
  data <- x$get()
  # Invert it
  m <- solve(data, ...)
  # Store it in the cache
  x$setinverse(m)
  # Return it
  m
}

