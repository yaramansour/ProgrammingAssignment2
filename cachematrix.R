
## function that set and get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    ##setting the matrix
  }
  get <- function() x
  ##getting the matrix data 
  setInverse <- function(inverse) inv <<- inverse
  ## setting the inverse of the matrix
  getInverse <- function() inv
  ## get the inverse of the matrix that's already calculated 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## function that calculate the inverse of a matrix if not calculated before
## and just gets if if it's calculated before 

cacheSolve <- function(x, ...) {
  #takes the value of inv from getinverse in the previous function 
  inv <- x$getInverse()
  #compare the value of ive 
  #if NOT equal to nul it means tha the inverse is calculated before for this matrix
  if (!is.null(inv)) {
    message("cached data")
    return(inv)
  }
  # else it gets the data of the matrix with function get()
  mat <- x$get()
  # then use solve() to get the inverse of the matrix
  inv <- solve(mat,...)
  # set the inv to the value calculated by solve
  x$setInverse(inv)
  #return the inverse
  message("calculating ..")
  inv
}
