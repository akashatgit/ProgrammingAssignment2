## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL  ##initially setting m to null
  set <- function(y){    ## the set function which assigns the value to the newly created object
    x <<- y
    m <<- NULL    ## sets x to the given matrix and m to null
  }
  
  get <- function() x   ## get function to return the value of the object
  setinverse <- function(inverse = numeric()) m <<- inverse  ##  function to set the inverse 
  getinverse <- function() m ## function to get the inverse
  list(set = set, get = get, setinverse = setinverse , getinverse = getinverse) ## list of all function

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

 m <- x$getinverse() ## taking the cached value
  if(!is.null(m)){ ## checking the nullness of the cached and value (if not null then using the value)
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##taking the data
  m <- solve(data) ## since cached value is null so calculating the inverse
  x$setinverse(m)  ## caching the calculated value
  m
        ## Return a matrix that is the inverse of 'x'
}
