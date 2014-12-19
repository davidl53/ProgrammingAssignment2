## These functions calculate the inverse of a matrix, making the
## assumption that the matrix has an inverse. The result is cached
## and the cached result accessed if the inverse of the same matrix
## is required again.

## makeCacheMatrix creates a list of the functions later called by
## cacheSolve and stores the matrix. It also initialises the inverse (i) 
## to NULL 

makeCacheMatrix <- function (x = matrix()) {
  i <- NULL
  set <- function(y){
        x <<- y
        i <<- NULL
        }
  get <- function() x
  setinverse <- function(solve) i <<-solve
  getinverse <- function() i
  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse) 
}


## cacheSolve uses the object created in makeCacheMatrix to either 
## calculate or retrieve the matrix inverse

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)        ## gets previously calculated inverse
  }
  data <- x$get()
  i <- solve(data, ...)  ## calculates inverse
  x$setinverse(i)        ## caches inverse 
  i
  
}
