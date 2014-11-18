## The function stores a matrix which is invertible.
## If the given matrix is not invertible, the function returns the sentence "Your matrix is not invertible. The determinant is equal 0."

makeCacheMatrix <- function(m = matrix()) {
  if (det(m)!=0){
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
  } else { print("Your matrix is not invertible. The determinant is equal 0.")}
  
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