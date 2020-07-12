## Example: Computing the inverse of the matrix
## Matrix inversion is usually computationally intensive, hence there might be some benefits in caching the inverse of a matrix rather than performing repeated computation.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##define the return matrix object by defining it as 'get'
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## ## ## This cacheSolve function computes the inverse of the special "matrix" created by
## makeCacheMatrix  function above. However,it will first checks to see if the inverse has already been calculated. If the inverse has already been calculated it gets the inverse from the cache and skips the computation(and the matrix has not changed), then it should retrieve the inverse from the cache.

## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## The cacheSolve function performs the following:
## 1. Check if the inverse object exist and the matrix is unchanged by getting
##    the stored inverse matrix calling from makeCacheMatrix
## 2. Return the inverse object in cache if it satisfy the first rules.
## 3. Else, compute the inverse of the special 'matrix', store in
##    makeCacheMatrix for future use and return the new inverse object.

###TEST RUN THE FUNCTION
# Test 1
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

# Test 2
B <- matrix(c(4, 3, 3, 2), 2, 2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

# Test 3
B <- matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), 3, 3)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)



#### It should be noted that if the matrix provided is a singular matrix, inessence it has value for determinant equal zero (0), then the matrix inverse will not ne computed and an error message will be produced. Hence, makeCacheMatrix() will produce a logical False response.
