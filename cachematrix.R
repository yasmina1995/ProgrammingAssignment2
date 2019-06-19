## There are two functions, one called makeCacheMatrix and the other called CacheSolve. You will seee what they
##both do below

## First function runs to get the result and the second if the result is already calculates it gets it directly
##from the cache.


##This function creates a special "matrix" object that can cache its inverse.FIrst created a variable called a
##and set the function. Then assigned x and n in a different environment then the current environment. 
##The assigned a function to get, setinverse and getinverse, then applied them in a list function.

makeCacheMatrix <- function(x = matrix()) {
  
  a <- NULL
  set <- function(y) {
    
    x <<- y
    a <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##The second function computes the inverse of the special "matrix" returned by "makeCacheMatrix"
##First created a variable that will get the inverse of x. If the a variable is not null then will get the 
##cached data and return a from there isn't of running again. If does not find it will find run to get the result

cacheSolve <- function(x, ...) {
 
        ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  
  if (!is.null(a)) {
    
    message("getting cached data")
    return(a)
    
  }
  
  mat <- x$get()
  a <- solve(mat, ...)
  x$setInverse(a)
  a
  
}
