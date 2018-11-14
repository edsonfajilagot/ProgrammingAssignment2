## The 2 function below is used to get the inverse of the matrix and using caching technique.

# This function creates a special "matrix" object which is a list containing 
# function to:
# 1. set the value of the matrix
# 2. get the matrix
# 3. set the value of inverse of a matrix
# 4. get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y){
    x <<- y
    m_inv <<- NULL
  }
  
  get <- function() x
  set_inv <- function(inv) m_inv <<- inv
  get_inv <- function() m_inv
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)

}


# This function calculates the inverse of a matrix.
# It first check if the inverse matrix has been already been calculated
# Otherwise it will calculate the inverse of the matrix and save it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inv()
  if(!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data)
  x$set_inv(m_inv)
  m_inv
}
