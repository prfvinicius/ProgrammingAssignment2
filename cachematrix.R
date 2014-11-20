## The function stores in cache a matrix which is invertible.
## The first step is to identify that the matrix is invertible. If the given matrix is not invertible, the function returns the sentence "Your matrix is not invertible. The determinant is equal 0 or the matrix is not square"

makeCacheMatrix <- function(m = matrix()) {
  if (det(m)!=0 & nrow(m)==ncol(m)){
    i <- NULL
    set <- function(matrix) {
      m <<- matrix
      i <<- NULL
    }
    get <- function() {
      m
    }
    setinverse <- function(inverse) {
      i <<- inverse
    }
    getinverse <- function(){
      i
    } 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  } else { print("Your matrix is not invertible. The determinant is equal 0 or the matrix is not square.")}
  
}



## This function returns the inverse matrix of the given matrix stored in cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  m
}