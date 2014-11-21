## Write functions to cache inverse of a matrix

## Create a maxtrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {  ##input x will be a matrix
    ## m resets to NULL every time makeVector is called
    m<-NULL
    
    ##set the matrix
    set<-function(y){
      x <<- y
      m <<- NULL
    }
    
    ##get the matrix
    get <- function() {x}  
    
    ##set the inverse of the matrix
    setInverse <- function(solve) {m <<- solve}
    
    ##get the inverse of the matrix
    getInverse <- function() {m}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 
  
}


## Compute the inverse of the matrix returned by makeCacheMatrix function above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ##Return cached data if already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##get matrix
  data <- x$get()
  
  ##get inverse of matrix
  m <- solve(data, ...)
  
  x$setInverse(m)
    
  #return inverse of matrix
  m
}
